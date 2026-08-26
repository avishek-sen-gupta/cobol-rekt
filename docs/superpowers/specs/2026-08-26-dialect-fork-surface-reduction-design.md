# Reducing the Fork's Upstream Surface for Dialect Grafting

**Date:** 2026-08-26
**Submodule baseline:** upstream 2.5.1, submodule branch `merge-2.5.1` (commit `a11a30f1e`)
**Status:** design approved, ready for implementation planning

## 1. Problem

smojol needs a parse tree containing both COBOL AST nodes and dialect (IDMS, CICS, DB2 SQL) AST
nodes. Upstream che4z discards dialect subtrees: each dialect visitor blanks its fragment out of the
extended document so the COBOL parser sees only spaces, and the dialect's own parse tree is dropped
once its `Node`s are built.

The fork recovers those subtrees by injecting a `_DIALECT_ <guid>` marker into the blanked region,
stashing the dialect parse tree in a static registry keyed by that guid, and re-grafting it post-parse
as a `DialectContainerNode` child of the matching `DialectNodeFillerContext`.

Making the marker lex and parse costs grammar changes in four files. Recording the guid on parse-tree
nodes costs an `AnnotatedParserRuleContext` superclass wired into three dialect grammars. And because
the substitution had to be woven into each dialect visitor, two upstream visitors were copied
wholesale into the fork rather than extended.

That last point has already cost correctness. `CicsSubstitutingVisitor` was copied from an older
`CICSVisitor` and has since drifted: it is missing `CICSOptionsCheckUtility` and
`CICSCheckUtilityParameters` (all CICS option validation), `cics_abend` handling and
`ExecCicsAbendNode`, the `cicsParser.missingEndExec` diagnostic, and `visitVariableNameUsage`. CICS
analysis under smojol is quietly weaker than upstream's, and the gap widens with every release.

### 1.1 Measured baseline

Modified upstream main-source files in the submodule versus tag 2.5.1 (fork-*added* files excluded —
they cost nothing on the metric this design optimises):

| File | Lines |
|---|---|
| `server/common/.../common/AnalysisConfig.java` | +106/−0 |
| `server/parser/.../core/CobolParser.g4` | +85/−18 |
| `server/dialect-idms/.../idms/IdmsParser.g4` | +48/−6 |
| `server/dialect-idms/.../idms/IdmsVisitor.java` | +44/−10 |
| `server/engine/.../implicitDialects/sql/Db2SqlDialect.java` | +37/−6 |
| `server/engine/.../implicitDialects/cics/CICSDialect.java` | +28/−4 |
| `server/engine/.../core/visitor/CobolVisitor.java` | +19/−16 |
| `server/dialect-idms/.../idms/IdmsDialect.java` | +18/−0 |
| `server/engine/.../dialects/ibm/ParserStage.java` | +12/−13 |
| `server/engine/.../dialects/DialectService.java` | +11/−1 |
| `server/dialect-idms/.../idms/IdmsLexer.g4` | +5/−0 |
| `server/parser/.../core/CobolLexer.g4` | +5/−2 |
| `server/engine/.../implicitDialects/sql/Db2SqlVisitor.java` | +2/−1 |
| `server/engine/.../settings/AnalysisConfigHelper.java` | +2/−1 |
| `server/engine/.../communications/ServerCommunications.java` | +2/−1 |
| `server/parser/.../parser/AntlrCobolParser.java` | +2/−1 |
| `server/engine/.../implicitDialects/cics/CICSParser.g4` | +1/−1 |
| `server/engine/.../implicitDialects/sql/Db2SqlParser.g4` | +1/−1 |
| `server/engine/.../implicitDialects/cics/CICSVisitor.java` | +1/−1 |
| `server/dialect-idms/.../idms/CobolErrorStrategy.java` | +1/−1 |

**20 files, +430/−84 = 514 affected lines.**

Fork-owned copies of upstream visitors: `CicsSubstitutingVisitor` 364 lines,
`Db2SqlSubstitutingVisitor` 510 lines — 874 lines total.

## 2. Goal and metric

**Primary metric: minimise the number of modified upstream files and lines.** Every patched upstream
file is a merge conflict at each release and a place where upstream improvements can be silently
lost.

Secondary: eliminate the copied visitors, which are the mechanism by which upstream improvements are
already being lost.

Targets:

- 20 modified upstream files → **14** (15 if the IDMS grammar reshape in §5.3 proves infeasible)
- +430/−84 → **~+301/−63**
- Dialect-substitution-attributable surface: ~172 lines across 12 files → **~42 lines across 5 files**
- Dialect-attributable *grammar* surface: 36 lines across 5 files → **6 lines in 1 file**
- Fork-owned visitor copies: 874 lines → ~60

## 3. Non-goals

- **Replacing the static `PersistentData` with a per-parse instance registry.** Considered and
  explicitly dropped. The one-line `@Builder.Default` field on `DialectProcessingContext` is cheap,
  but the read side is not: smojol grafts at `ParsePipeline.java:160` and nothing in `smojol-core` or
  `smojol-toolkit` can reach a `DialectProcessingContext`. The static is what currently carries
  fragments across the che4z→smojol boundary; replacing it means new patches on `ParserStage`,
  `ParserStageResult` or `AnalysisContext`. That is the only change in this set that would *increase*
  the metric. `PersistentData.java` is a fork-added file, so rewriting its internals is free.
  Consequence accepted: the not-thread-safe constraint in `COBOL-LSP-INTEGRATION.md` §5 stands, and
  parallel parsing remains blocked.
- **The ~65 lines of smojol-motivated named-rule extraction in `CobolParser.g4`** (`divisor`,
  `multiplyLhs`, `charBasedOperator`, `additionalCondition`, `multipleArithmeticExpressions`,
  `nextSentence`/`nextSentenceWrapperStatement`, `mapStatement`,
  `genericOnClauseStatement`/`onClauseBlock`, and hoisting `NEXT SENTENCE` out of
  `ifThen`/`ifElse`/`WHEN`). These exist so smojol has named contexts to attach flow nodes to. Not
  dialect work, not upstreamable, out of scope.
- **The ~60 lines of new IDMS statement support in `IdmsParser.g4`/`IdmsLexer.g4`** (`ADD`/`DEL
  MODULE`/`RECORD`, `LANGUAGE IS COBOL`, `MODULE SOURCE`, the mandatory-terminator `transferStatement`
  split). Real functionality, out of scope.
- **Upstreaming anything.** Several changes here are plausible upstream PRs (§9). Filing them is
  follow-on work, not part of this spec.

## 4. Design: replace the marker with positional correlation

### 4.1 Principle

Blanking is length-preserving: upstream's `changeContextToDialectStatement` calls
`fillArea(range, FILLER)` and its `addReplacementContext` replaces each terminal node's text with
filler of equal length. So a fragment's position in the extended document is identical before and
after substitution. That makes position a sufficient key — the injected marker is redundant.

The single exception is IDMS's `_IF_ ` prefix, which is not length-preserving. It is upstream's own
mechanism (`IdmsVisitor` already declares `IF = "_IF_ "` and calls
`addReplacementContext(ctx, IF)`), it is required for `dialectIfStatment` to match, and it is
retained. The spike confirmed correlation still succeeds across it, including on a multi-line
fragment recorded at `[12:11..13]`.

### 4.2 `PersistentData` rewrite

`PersistentData` remains static (see §3). Its contents change from a guid-keyed tree lookup to a
positional fragment list:

```java
public static final class Fragment {
    public final int startLine, startChar, endLine;
    public final LocalisedDialect dialect;
    public final ParseTree tree;
    public boolean covers(int line, int charPos) { ... }
}
public static void record(ParserRuleContext ctx, LocalisedDialect dialect);
public static boolean isCovered(int line, int charPos);
public static Fragment claim(int line, int charPos);
public static void reset();
```

`claim` marks the returned fragment consumed, in an identity set, so a fragment is grafted at most
once. `reset()` clears both the list and the claimed set and **must** be called at `ParsePipeline`
entry. The spike found sequential parses succeeded only incidentally, because the claimed set skipped
positionally identical fragments left over from the previous parse; an explicit reset makes that
deliberate rather than accidental.

Removed: `counter`, `next()`, `addDialectTree`, `getDialectNode`, `dialect(String)`, `tree`, `trees`.
`treeCount()` is test-only and goes with them.

Nesting needs no handling. IDMS fragments cannot nest: `idmsIfStatement : inquireMapIfStatement` is
disjoint from `idmsIfCondition`, verified by grammar inspection, empirically during the spike (an
`IF NOT IX-EMP MEMBER ... ELSE ... END-IF` fixture produced exactly one fragment), and corroborated
by an upstream test comment stating `visitIdmsIfCondition` must not fire for `inqMapIfPhrase`.

### 4.3 smojol-side readers

Four call sites move from guid to position:

| File | Now | After |
|---|---|---|
| `smojol-core/.../idms/DialectIntegratorListener.java` | `ctx.dialectGuid()` → `getDialectNode("IDMS-" + guid)` | `PersistentData.claim(line, charPos)` |
| `smojol-core/.../dialect/LanguageDialect.java` | `filler.whatever() != null` | `!PersistentData.isCovered(line, charPos)` |
| `smojol-toolkit/.../ast/CompositeCobolFlowNode.java` (`isNullDialectNode`) | guid presence | positional lookup |
| `smojol-core/.../ast/NodeText.java` (`dialectOriginalText`) | `CobolParser.DialectGuidContext` | positional lookup |

`smojol-core/src/test/.../DialectIntegratorListenerMissingKeyTest.java` mocks `dialectGuid()` and
must be rewritten against the positional API.

### 4.4 Consequences

`AnnotatedParserRuleContext` and its `customData` map lose every non-test reader. That removes the
`contextSuperClass` option from `IdmsParser.g4`, `CICSParser.g4` and `Db2SqlParser.g4`, and kills
`setDialectRecursively` — an 11-line method duplicated verbatim in both `CICSDialect` and
`Db2SqlDialect` plus `IdmsDialect`, which casts every node to `AnnotatedParserRuleContext` to stamp a
dialect that is now recorded once per fragment.

## 5. Design: reparent the substituting visitors

### 5.1 Mechanism

Each fork visitor extends its upstream counterpart and overrides only the public `visit*` methods
that substitute, recording the fragment and delegating to `super`. No `protected` widening is
required: the subclass never calls upstream's private helpers, it just lets them run.

```java
class CicsSubstitutingVisitor extends CICSVisitor {          // was: extends ErrorHandlingCICSVisitor
  CicsSubstitutingVisitor(DialectProcessingContext context, MessageService messageService) {
    super(context, messageService);
  }
  @Override
  public List<Node> visitCicsExecBlock(CICSParser.CicsExecBlockContext ctx) {
    PersistentData.record(ctx, LocalisedDialect.CICS);
    return super.visitCicsExecBlock(ctx);
  }
  // + visitCicsDfhResp
}
```

Recording before `super` is safe: the range comes from the parse tree, not from the document, so it is
unaffected by the blanking `super` performs.

All three visitors and their upstream counterparts are package-private, so each subclass lives in its
counterpart's package. Call-site correspondence is exact:

| Dialect | Upstream substitution site | Fork site today |
|---|---|---|
| CICS | `visitCicsExecBlock` → `changeContextToDialectStatement` (`CICSVisitor:99`) | `replaceWithMetadata` (`:86`) |
| CICS | `visitCicsDfhResp` → `addReplacementContext` (`:175`) | `replaceWithMetadata` (`:131`) |
| CICS | `visitCicsDfhValue` → `addReplacementContext` (`:202`) | commented out (`:158`) |
| CICS | `visitAllExciRules` → *no substitution* | `addReplacementContext` (`:159`) |
| DB2 | `visitExecRule` → `addReplacementContext` (`Db2SqlVisitor:71`) | `replaceWithMetadata` (`:87`) |
| DB2 | `visitLob_host_variables_arrays` → `addReplacementContext` (`:218`) | `replaceWithMetadata` (`:212`) |
| DB2 | `visitBinary_host_variable` → `addReplacementContext` (`:246`) | `replaceWithMetadata` (`:233`) |
| IDMS | `visitIdmsStatements`, `visitIdmsSections`, `visitIdmsIfStatement`, `visitIdmsIfCondition` → `addReplacementContext` | `replaceWithMetadata` |
| IDMS | `visitSchemaSection` → *no substitution* | `replaceWithMetadata` |

Override counts after the change: CICS 2, DB2 3, IDMS 5.

### 5.2 Deletions

`ErrorHandlingCICSVisitor` (34 lines) and `MarkerDb2SqlVisitor` (32 lines) are fork-added abstract
classes whose sole content is `protected final List<SyntaxError> errors`. They exist only to give the
original and substituting visitors a common supertype. Direct subclassing makes them unnecessary, and
their deletion reverts `CICSVisitor.java` (+1/−1) and `Db2SqlVisitor.java` (+2/−1) to pristine.

Both upstream visitors currently declare their own `@Getter private final List<SyntaxError> errors`,
shadowing the inherited field — a latent hazard that disappears. `MarkerDb2SqlVisitor` also currently
lives in the `cics` package while extending `Db2SqlParserBaseVisitor` from `sql`; that too goes.

`CICSVisitorBuilder` and `Db2SqlVisitorBuilder` are fork-owned. Their `visitor(...)` return types
change from the deleted abstract classes to `CICSVisitor` and `Db2SqlVisitor`.

### 5.3 IDMS: a grammar constraint

IDMS is the largest reparent (`IdmsVisitor.java` is +44/−10) and the only one with an obstacle.
Upstream's `visitIdmsStatements` reads:

```java
if (ctx.imperativeStatementCall() == null) addReplacementContext(ctx);
else addReplacementImperativeStatementContext(ctx, ctx.imperativeStatementCall());
```

The fork restructured the rule:

```antlr
// upstream 2.5.1
idmsStatements : idmsStmtsOptTermOn imperativeStatementCall? ;
// fork
idmsStatements : idmsOptTermStatement | idmsMandTermStatement ;
```

`IdmsStatementsContext.imperativeStatementCall()` therefore no longer exists, which is why the fork
added `imperativeStatementCallOf()` walking via `idmsOptTermStatement()`/`idmsMandTermStatement()`.
Reverting `IdmsVisitor.java` to pristine would fail to **compile**, not merely misbehave.

**Primary approach:** reshape `idmsStatements` so `imperativeStatementCall` remains a direct child,
preserving the generated accessor while keeping the fork's new statements and optional/mandatory
terminator distinction.

**Hard constraint:** `imperativeStatementCall` must appear **exactly once** in the rule. Two or more
occurrences make ANTLR generate `List<ImperativeStatementCallContext> imperativeStatementCall()`
instead of a scalar accessor. Upstream's `ctx.imperativeStatementCall() == null` would still compile
against a `List` but would always be false, silently taking the wrong branch — a worse outcome than
the compile error. Any candidate shape must be checked against the generated parser, not just eyeballed.

**Fallback:** if no shape satisfies both, keep a minimal `IdmsVisitor` patch — `imperativeStatementCallOf`
plus the one-line `visitIdmsStatements` override, roughly +6/−2 instead of 0. The metric then lands at
15 files, +307/−65.

Fork additions to `IdmsVisitor` that move to the subclass or are deleted outright:

- `replaceWithMetadata` (both overloads) → subclass records, delegates to upstream's
  `addReplacementContext`
- `SCHEMA_SECTION = "_SCHEMA_ "` → deleted; `visitSchemaSection` substitutes with no prefix
- `extractions` counter → subclass
- `visitIdmsStmtsOptTermOn` → deleted; it does nothing but call `super`
- `IF` and `addReplacementContext(ctx, prefix)` → **not** fork code; already upstream, left alone

### 5.4 Dialect-class hookups

`IdmsDialect` instantiates the visitor directly at `IdmsDialect.java:227`
(`new IdmsVisitor(context)`, currently unpatched) and needs a one-line swap to the subclass. All 18 of
its current insertions die: five `poc`/`antlr` imports, a stray `System.out.println` reporting
extraction counts, `PersistentData.addDialectTree`, and the `setDialectRecursively` call plus method.
Net +18/−0 → +1/−1.

Unlike CICS and DB2, IDMS gets no builder seam and no config flag — the fork always substitutes for
IDMS today, and this design preserves that. Adding a seam would cost more upstream lines than it saves.

`CICSDialect` (+28/−4 → ~+9/−3) and `Db2SqlDialect` (+37/−6 → ~+10/−3) keep their `visitorBuilder`
field, extra constructor and visitor-selection line; they shed the `poc`/`antlr` imports,
`setDialectRecursively`, `addDialectTree`, and a dead commented-out line. `Db2SqlDialect`'s patch also
contains ~10 insertions of pure import reorganisation (expanding `java.util.*` and `copybook.*` into
explicit imports) unrelated to any of this — revert it as a free win.

`DialectService`'s +11/−1 is irreducible: something must choose between the original and substituting
visitors, gated on `config.isAddCicsPlaceholder()` / `isAddDb2SqlPlaceholder()`.

## 6. Design: grammar

`CobolLexer.g4` reverts to pristine. Four fork-only tokens go: `DIALECT_MARKER_LITERAL`, `SCHEMA`,
`DIALECT_SCHEMA_SECTION`, `DIALECT_MARKER`.

`CobolParser.g4`'s dialect portion (~+20/−3) reduces to three lines:

```antlr
conditionalStatementCall : statement | dialectStatement ;
dialectStatement         : dialectIfStatment | dialectNodeFiller ;
dialectNodeFiller        : ZERO_WIDTH_SPACE+ DOT_FS? EOF? ;
```

`dialectSectionBlock`, `dialectGuid`, `whatever`, `eater` and a commented-out `dialectNodeFiller`
corpse are all deleted, and `dialectSection : dialectNodeFiller` reverts to upstream.

**The `dialectStatement` reorder is load-bearing and non-obvious.** Upstream's rule is
`ZERO_WIDTH_SPACE | dialectIfStatment` — the bare token alternative comes first, so a blanked
procedure-division statement matches a bare token and produces no rule context to graft onto. The
spike recovered 1 of 4 fragments before this reorder and 4 of 4 after. Dropping the bare
`ZERO_WIDTH_SPACE` alternative and letting `dialectNodeFiller` absorb it is what makes the
marker-free design work at all.

`contextSuperClass = ...AnnotatedParserRuleContext` comes out of `CICSParser.g4`, `Db2SqlParser.g4`
and `IdmsParser.g4` (§4.4).

Resulting dialect-attributable grammar surface: **36 lines across 5 files → 6 lines in 1 file.** Total
grammar surface 173 → ~143 lines, 6 files → 3, the residue being smojol named-rule extraction, new
IDMS statement support, and genuine COBOL grammar fixes (§3).

The value here is not the line count. It is that dialect grafting stops being a *reason* the grammars
are forked. The dialect rules are the ones upstream actively develops, and they are what forced step 4
of `COBOL-LSP-INTEGRATION.md` §3 ("re-apply `dialectNodeFiller` grammar insertions if upstream changed
surrounding rules").

## 7. Two behaviour divergences to settle

The copied visitors diverge from upstream in two places, both untested, both requiring an explicit
decision during implementation:

1. Fork substitutes in `visitAllExciRules`; upstream does not.
2. Fork skips `visitCicsDfhValue`; upstream substitutes there.

**Default: adopt upstream's behaviour for both**, i.e. do not override `visitAllExciRules`, and do
override `visitCicsDfhValue` to record. Reparenting makes upstream's behaviour the default anyway, so
this is the low-effort path. If either flips a test, the test decides and the deviation gets an
explanatory comment. Neither may be left implicit.

## 8. Testing

### 8.1 The coverage gap that caused the drift

`DialectService:212` selects the substituting visitor only when `config.isAddCicsPlaceholder()` is
set. All 5177 che4z engine tests run the **original** visitor, so the substituting path has no che4z
coverage whatsoever. That is why `CicsSubstitutingVisitor` could fall behind upstream unnoticed.

This spec adds che4z-side tests that flip the flag and assert the substituting visitor produces the
same `Node`s and the same errors as the original, plus a recorded fragment. Without these, the
reparent is verified only indirectly through smojol.

### 8.2 Existing gates

Established green during the spike with a marker-free implementation:

| Suite | Result |
|---|---|
| che4z `engine` (CICS + DB2 SQL, original path) | 5177 tests, 0 failures |
| che4z `dialect-idms` | 419/419 parsing tests; only `TestPersistentDataExtraction`'s 12 guid assertions failed |
| `smojol-core` | 108, 0 failures |
| `smojol-toolkit` `IdmsDialectIntegrationTest` | 9/9 |

`IdmsDialectIntegrationTest` is the load-bearing suite: it asserts the exact four-node count,
`LocalisedDialect.IDMS` annotation, marker-free node text, `DialectNodeFillerContext` parentage,
one child per filler, and stability across sequential parses.

`TestPersistentDataExtraction`'s 12 assertions test the guid mechanism itself and must be rewritten
against positions and fragment counts. They are the only expected casualties.

### 8.3 Build notes

Every Maven invocation needs `MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore"` (Cloudflare
Gateway TLS interception). Surefire filtering across this reactor needs both
`-DfailIfNoTests=false` and `-Dsurefire.failIfNoSpecifiedTests=false`; the two modules use different
surefire versions and each rejects a different flag. `smojol-toolkit` cannot be built with `-pl`
alone — it needs `-am` for `mojo-common:1.0-SNAPSHOT`.

## 9. Sequencing

1. **Marker-free positional correlation.** `PersistentData` rewrite, four smojol readers, grammar
   reduction in `CobolLexer.g4`/`CobolParser.g4`, `contextSuperClass` removal from all three dialect
   grammars, `setDialectRecursively` deletion. Green gates: §8.2 plus rewritten
   `TestPersistentDataExtraction`.
2. **Reparent CICS and DB2.** Delete `ErrorHandlingCICSVisitor` and `MarkerDb2SqlVisitor`, revert
   `CICSVisitor.java` and `Db2SqlVisitor.java`, collapse both copies to thin subclasses, settle §7,
   add the flag-flipped tests from §8.1.
3. **Reparent IDMS.** Attempt the `idmsStatements` reshape from §5.3; fall back to the minimal patch
   if the single-occurrence constraint cannot be met.
4. **Update `COBOL-LSP-INTEGRATION.md`** — §2's patch inventory (`AnnotatedParserRuleContext` and the
   `CobolLexer.g4` token row both go), §3's re-apply step, §5's constraint restated with the explicit
   `reset()` requirement.

### 9.1 Branching

All work happens on a named branch in each repository, never directly on `poc` or `main`:

| Repository | Branch | Based on | Merges to |
|---|---|---|---|
| `che-che4z-lsp-for-cobol-integration` (submodule) | `dialect-surface-reduction` | `merge-2.5.1` (`a11a30f1e`) | `poc` |
| `cobol-rekt` (parent) | `dialect-surface-reduction` | this spec's commit | `main` |

Same branch name in both repositories so the two halves are trivially correlated: the submodule branch
carries the che4z changes (§4.2, §5, §6), the parent branch carries the smojol-side reader changes
(§4.3) and the submodule pointer bump.

The submodule branch is based on `merge-2.5.1`, not `poc`, because every file position and line number
in this design is relative to upstream 2.5.1. Consequence: merging `dialect-surface-reduction` into
`poc` also brings the 2.5.1 merge along. Land `merge-2.5.1` on `poc` first so the two merges can be
reviewed and reverted independently.

Do not bump the parent's submodule pointer until the submodule branch has merged to `poc`.

Step 1 must land before 2 and 3: the reparent only collapses to "record, then delegate" once the
marker is gone. With the marker still present the subclass would have to re-implement the
substitution to inject a prefix, which defeats the purpose.

Follow-on, not in scope: propose the three `CobolParser.g4` lines upstream. All three widen rules
upstream already owns — `ZERO_WIDTH_SPACE`, `DIALECT_IF`, `dialectNodeFiller`, `dialectIfStatment`,
`dialectSection` and `dialectStatement` all ship in 2.5.1 — so acceptance would take dialect-attributable
grammar surface to zero. The genuine grammar fixes (`ALTERNATE (RECORD KEY? | KEY)`,
`level88DataUsageClause`, `| ZERO`, `MEMBERS?`, `putReturnClause?`) are separately upstreamable.

## 10. Risks

| Risk | Mitigation |
|---|---|
| IDMS grammar reshape can't keep a scalar `imperativeStatementCall()` accessor | Fallback to a ~6-line `IdmsVisitor` patch (§5.3). Verify against the generated parser, not by inspection — a `List` accessor compiles but silently inverts the branch. |
| Adopting upstream's `visitCicsDfhValue` / `visitAllExciRules` behaviour changes output | Untested today either way. Land it behind the §8.1 tests so the change is observed rather than assumed. |
| Reparenting activates upstream code the fork has never run (`CICSOptionsCheckUtility`, `cics_abend`, `missingEndExec`) | This is the point of the change, but it may surface new diagnostics on smojol's fixtures. Expect and triage new CICS errors rather than suppressing them. |
| Missing `PersistentData.reset()` at pipeline entry silently corrupts sequential parses | Explicit reset plus the sequential-parse assertion already in `IdmsDialectIntegrationTest`. |
| Position collision between two fragments starting at the same line/column | Cannot occur for IDMS (fragments cannot nest, §4.2) and fragments are disjoint by construction elsewhere. `claim` consumes on first match, so a collision would drop a graft rather than duplicate one — detectable via the restore count. |
