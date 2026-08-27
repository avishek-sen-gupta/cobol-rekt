# COBOL LSP Fork Integration

## Section 1 — Upstream base

- **Upstream repo:** https://github.com/eclipse-che4z/che-che4z-lsp-for-cobol
- **Fork branch:** `poc` in `avishek-sen-gupta/che-che4z-lsp-for-cobol-integration`
- **Current upstream base:** 2.5.1
- **How to find the merge-base for future upgrades:** `git merge-base <upstream-tag> poc`

## Section 2 — poc patch inventory (must survive every future merge)

| File | What | Why |
|---|---|---|
| `server/common/src/.../poc/PersistentData.java` | Static store for dialect parse-tree fragments | No DI container at parse time — static lookup replaces runtime injection |
| `server/common/src/.../poc/LocalisedDialect.java` | Dialect metadata carrier | Attached to positional fragment metadata |
| `CobolParser.g4` — surviving dialect grammar | Three lines: `conditionalStatementCall : statement \| dialectStatement ;`, `dialectStatement : dialectIfStatment \| dialectNodeFiller ;`, and `dialectNodeFiller : ZERO_WIDTH_SPACE+ DOT_FS? EOF? ;` | The alternative ORDER inside `dialectStatement` is load-bearing: upstream leads with a bare `ZERO_WIDTH_SPACE` alternative, which matches first and yields no rule context, so there is nothing to graft onto. Naming `dialectNodeFiller` first makes the filler run a rule context. The `+` is load-bearing too — one filler token per line for a multi-line fragment. |
| `server/engine/src/.../implicitDialects/cics/CicsSubstitutingVisitor.java` | Fork-added subclass of `CICSVisitor` (2 overrides) | Records positional fragment and delegates to `super`. Previously a verbatim copy that had silently lost `CICSOptionsCheckUtility` option validation, `cics_abend`/`ExecCicsAbendNode`, `missingEndExec` diagnostic, and `visitVariableNameUsage`, and whose own `constructRange` was missing upstream's `+1` (so its blanking was not length-preserving). |
| `server/engine/src/.../implicitDialects/sql/Db2SqlSubstitutingVisitor.java` | Fork-added subclass of `Db2SqlVisitor` (10 overrides) | Records positional fragment and delegates to `super`. Previously a patched copy that emitted `X(n)` instead of `G(n)` GRAPHIC for DBCLOB, never called `setSQLDecimalCommaAllowed`, and had the same missing `+1` in `constructRange`. Reparenting therefore *repairs* the length-preserving blanking that positional correlation depends on. |
| `server/dialect-idms/src/.../dialects/idms/IdmsSubstitutingVisitor.java` (note: `dialect-idms` module, not `engine`) | Fork-added subclass of `IdmsVisitor` (5 overrides, 2 of them conditionally non-delegating) | Records positional fragment. `visitIdmsSections`, `visitIdmsIfStatement` and `visitIdmsIfCondition` delegate to `super` unconditionally. `visitIdmsStatements` and `visitEraseStoreModifyLrStatementsOptions` delegate only when there is no `ON <path-status> <imperative>` clause; on that branch they substitute the clause themselves, behind an `_IF_ ` prefix (5 chars, the one deliberate exception to length-preserving substitution, tolerated because `PersistentData.Fragment.covers` is a range check). They must not delegate there: upstream's `addReplacementImperativeStatementContext` clears with plain spaces instead of `FILLER`, so it leaves no `ZERO_WIDTH_SPACE` anchor — and in the `visitEraseStoreModifyLrStatementsOptions` case it re-clears `ctx.getParent()`, destroying the anchor the enclosing statement had already written. |
| `IdmsParser.g4` — grammar reshape | Inlined `idmsOptTermStatement`/`idmsMandTermStatement` back into `idmsStatements` as two alternatives | Each sub-rule had a single reference; alternative priority unchanged. `imperativeStatementCall` now occurs at most once per alternative, so ANTLR emits a **scalar** accessor (verified against generated parser). This made `IdmsVisitor.java` pristine — no fork patches. |
| `server/engine/src/.../cli/di/CliModule.java` | Guice module | Wires parse engine without LSP server runtime |
| `server/engine/src/.../cli/modules/CliClientProvider.java` | Copybook path provider | Replaces LSP workspace-based copybook resolution |

## Section 3 — How to merge a future upstream release

1. `git merge-base <new-upstream-tag> poc` → extract poc-only grammar diff
2. Merge new tag into `poc`
3. Resolve conflicts: keep ALL files in the poc patch inventory above
4. Build + test: `mvn clean install -f server/pom.xml`
5. Update this document's "Current upstream base" line

## Section 4 — Grammar migration patterns (2.2.0 → 2.4.3)

These rule changes required updates across smojol. If a future upstream merge changes the same rules again, the same smojol files will need fixing.

| Old grammar construct | New grammar construct | Migration pattern | smojol files affected |
|---|---|---|---|
| `procedureSection` (separate rule) | `sectionOrParagraph` (combined rule; `SECTION` token distinguishes sections from paragraphs) | Replace `ProcedureSectionContext` with `SectionOrParagraphContext`. **Must check `SECTION() != null`** wherever only sections are expected. Name via `ctx.cobolWord() != null ? ctx.cobolWord().getText() : ctx.integerLiteral(0).getText()` | `CobolFlowNodeFactory`, `CobolFlowNode`, `SectionFlowNode`, `CobolEntityNavigator`, `PerSection`, `FlowchartGenerationStrategy` |
| `tableCall` → direct `arithmeticExpression` list | `tableCall` → `argument` wrapper → `arithmeticExpression` | `.argument().stream().map(ArgumentContext::arithmeticExpression).toList()` | `CobolExpressionBuilder`, `GeneralIdentifierVisitor`, `CobolReferenceBuilder` |
| `performUntil` → direct `condition` | `performUntil` → `performUntilCondition` → `condition` | Add `.performUntilCondition()` before `.condition()` | `FlowIterationBuilder` |
| `functionCall` → direct `functionName` | `functionCall` → `functionReference` → `functionName` | Add `.functionReference()` before `.functionName()` | `FunctionCallExpression`, `FunctionCallCobolReference` |
| `dataPictureClause` → `pictureString+` (list) | `dataPictureClause` → `pictureString` (singular) | Remove `.getFirst()` — access `pictureString()` directly | `CobolDataStructure`, `Format1DataStructure` |
| `CobolDataDivisionParser` (separate grammar) | Merged into `CobolParser` | Change import from `CobolDataDivisionParser` to `CobolParser` | `GenericProcessingFlowNode` |

### Checklist for future grammar upgrades

1. `grep -rn 'CobolParser\.[A-Z][A-Za-z]*Context' smojol-core/ smojol-toolkit/` — inventory all context types in use
2. For each context type, verify the grammar rule still exists: rule `fooBar` generates `FooBarContext`
3. Watch for rules that were split (new intermediate wrappers) or merged (combined contexts needing token-based discrimination)
4. Run `mvn test -pl smojol-core,smojol-toolkit` — all tests must pass before committing

## Section 5 — Architectural constraints

`PersistentData` holds a static fragment list and a static claimed set. It is not thread-safe by design, and `PersistentData.reset()` **must** be called at `ParsePipeline` entry — which it now is. Without the reset, a sequential parse can claim a positionally identical fragment left over from the previous parse.

**Known upstream defect** (pre-existing, not fork-introduced): `server/common/src/main/java/org/eclipse/lsp/cobol/common/mapping/MultilineReplaceStrategy.java:48` — `for (int i = 1; i < newLines.length - 2; i++)` never inserts the middle line of a 3-line replacement (and drops one for a 4-line one), so `ExtendedDocument.replace` over a range spanning three or more lines silently **drops lines**. Verified pristine against 2.5.1. Real IDMS source containing a 3-or-more-line substituted construct would silently break positional correlation. This is a candidate for an upstream PR; patching it in the fork would add upstream patch lines for a defect that is not the fork's.
