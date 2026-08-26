# Dialect Fork Surface Reduction Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Cut the `che-che4z-lsp-for-cobol-integration` fork's patched-upstream surface from 20 files / 514 lines to ~14 files / ~364 lines by replacing the `_DIALECT_ <guid>` marker with positional fragment correlation and reparenting the three copied dialect visitors onto their upstream counterparts.

**Architecture:** Upstream's dialect visitors already blank each dialect fragment out of the extended document, and blanking is length-preserving. So the extended-document position of a fragment is identical before and after substitution, which makes position a sufficient key and the injected marker redundant. Once the marker is gone, each fork visitor collapses to "record the fragment's position and parse tree, then delegate to `super`" — which needs no upstream edits at all, because the fork subclass lives in the same package as its upstream counterpart and only overrides public `visit*` methods.

**Tech Stack:** Java 17, Maven multi-module reactor, ANTLR 4 (`.g4` parser/lexer grammars, generated `*Context` classes), Lombok, JUnit 5, Mockito, Guice.

**Spec:** `docs/superpowers/specs/2026-08-26-dialect-fork-surface-reduction-design.md`

## Global Constraints

- **Two repositories.** The submodule `che-che4z-lsp-for-cobol-integration/` is a separate git repository from the parent `cobol-rekt`. Every task states which one it commits to. Commit in the submodule first, then the parent.
- **Branches.** Both repositories already have a branch `dialect-surface-reduction` checked out. Submodule branch is based on `merge-2.5.1` (`a11a30f1e`); parent branch is based on `bf67fbf`. Never commit to `poc` or `main`.
- **Do not bump the parent's submodule pointer** (`git add che-che4z-lsp-for-cobol-integration`) until Task 9. Tasks 1–8 leave the pointer alone.
- **Every Maven invocation** needs `MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore"` (Cloudflare Gateway TLS interception). Without it, dependency resolution fails with a TLS error.
- **Surefire test filtering** across this reactor needs both `-DfailIfNoTests=false` and `-Dsurefire.failIfNoSpecifiedTests=false`. The two modules use different surefire versions and each rejects a different flag.
- **`smojol-toolkit` cannot be built with `-pl` alone** — it needs `-am` to resolve `mojo-common:1.0-SNAPSHOT`.
- **Parent builds against an installed submodule.** After any submodule change, run `mvn install -DskipTests` in the submodule's `server/` before building the parent, or the parent compiles against stale classes.
- **Talisman pre-commit hook stays active.** Never use `git commit --no-verify`. If Talisman blocks a commit, add the filename+checksum entry it prints to that repository's `.talismanrc`, inserted before the trailing `version: "1.0"` line. Only the first entry per filename is honoured — update an existing entry rather than appending a duplicate. Compute a checksum with `talisman --checksum=<path>`.
- **The optimisation metric is the number of modified upstream files and lines**, measured by `git diff --numstat --diff-filter=M 2.5.1 -- 'server/**/src/main/**'` in the submodule. Fork-*added* files cost nothing. Never solve a problem by patching an upstream file when a fork-added file will do.
- **Position convention.** All fragment positions use ANTLR token coordinates: `Token.getLine()` (1-based) and `Token.getCharPositionInLine()` (0-based). Never mix in `Range`/LSP coordinates, which are 0-based on both axes.
- **`PersistentData` stays static.** Replacing it with a per-parse instance registry was considered and explicitly rejected in spec §3 because the read side would require new patches on `ParserStage`/`ParserStageResult`/`AnalysisContext`.

## Three Corrections to the Spec

These were found while preparing this plan. Each one changes what a task does; they are recorded here so the executor does not "fix" the plan back towards the spec.

1. **Two extra fork-added grammars reference the marker tokens.** `server/parser/src/main/antlr4/.../CobolSentenceParser.g4` (2184 lines, fork-added) and `CobolExpressionParser.g4` (153 lines, fork-added) both declare `options {tokenVocab = CobolLexer;}` and contain `dialectNodeFiller : (DIALECT_MARKER dialectGuid DOT_FS? eater) | whatever`. The `antlr4-maven-plugin` has no include filter, so both are generated on every build. Reverting `CobolLexer.g4` without cleaning these two will fail or warn at generate-sources. Task 4 cleans all four grammars together. They are fork-added, so editing them costs nothing on the metric.

2. **The two CICS "behaviour divergences" in spec §7 do not exist.** `CicsSubstitutingVisitor.visitAllExciRules` (`:137`) is character-identical to `CICSVisitor.visitAllExciRules` (`:180`) — both are a commented-out block plus `visitChildren(ctx)`, and neither substitutes. And `CicsSubstitutingVisitor.visitCicsDfhValue` (`:157`) *does* substitute — it calls `addReplacementContext(ctx)`; only the `replaceWithMetadata` line is commented out. So the real difference is that the fork substitutes there without *recording* a fragment. Consequently the CICS subclass overrides exactly two methods (`visitCicsExecBlock`, `visitCicsDfhResp`) and nothing else, and that reproduces today's behaviour exactly. No decision is needed and no comment is required.

3. **IDMS `visitSchemaSection` must not record.** `idmsSections : idmsControlSection | schemaSection | mapSection`, so `schemaSection` is a direct child of `idmsSections`, and `visitIdmsSections` already substitutes and (after this work) records the enclosing fragment. The fork's extra `replaceWithMetadata(ctx, SCHEMA_SECTION + " ")` in `visitSchemaSection` is a second, nested substitution of the same region — which under positional correlation would create two fragments with the same start position, the exact collision spec §10 lists as a risk. Recording only at `visitIdmsSections` is strictly correct: the `schemaSection` subtree is already inside the recorded `idmsSections` tree. So the IDMS subclass overrides **four** methods, not five, and `SCHEMA_SECTION`, `DIALECT_SCHEMA_SECTION`, `SCHEMA`, and `dialectSectionBlock` all die together consistently.

## File Structure

### Submodule (`che-che4z-lsp-for-cobol-integration/`)

Fork-added files (free on the metric — prefer these):

| File | Responsibility after this work |
|---|---|
| `server/common/src/main/java/org/eclipse/lsp/cobol/common/poc/PersistentData.java` | Positional fragment registry: record, look up, claim, reset. Rewritten. |
| `server/common/src/main/java/org/eclipse/lsp/cobol/common/poc/LocalisedDialect.java` | Unchanged 3-value enum. |
| `server/common/src/main/java/org/eclipse/lsp/cobol/common/poc/AnnotatedParserRuleContext.java` | **Deleted** (Task 5). |
| `server/common/src/test/java/org/eclipse/lsp/cobol/common/poc/PersistentDataTest.java` | **New.** Unit tests for the positional API. |
| `server/engine/.../implicitDialects/cics/CicsSubstitutingVisitor.java` | Reduced from 364 lines to a ~30-line subclass of `CICSVisitor`. |
| `server/engine/.../implicitDialects/cics/ErrorHandlingCICSVisitor.java` | **Deleted** (Task 6). |
| `server/engine/.../implicitDialects/cics/CICSVisitorBuilder.java` | Return type changes to `CICSVisitor`. |
| `server/engine/.../implicitDialects/sql/Db2SqlSubstitutingVisitor.java` | Reduced from 510 lines to a ~40-line subclass of `Db2SqlVisitor`. |
| `server/engine/.../implicitDialects/cics/MarkerDb2SqlVisitor.java` | **Deleted** (Task 7). |
| `server/engine/.../implicitDialects/sql/Db2SqlVisitorBuilder.java` | Return type changes to `Db2SqlVisitor`. |
| `server/dialect-idms/.../idms/IdmsSubstitutingVisitor.java` | **New** (Task 8). ~60-line subclass of `IdmsVisitor`. |
| `server/engine/src/test/java/.../implicitDialects/cics/CicsSubstitutingVisitorEquivalenceTest.java` | **New** (Task 6). Closes the §8.1 coverage gap for CICS. |
| `server/engine/src/test/java/.../implicitDialects/sql/Db2SqlSubstitutingVisitorEquivalenceTest.java` | **New** (Task 7). Same for DB2. |
| `server/parser/src/main/antlr4/.../CobolSentenceParser.g4`, `CobolExpressionParser.g4` | Fork-added; dialect-marker rules stripped (Task 4). |
| `server/dialect-idms/src/test/.../usecases/TestPersistentDataExtraction.java` | Rewritten against positions (Task 4). |

Patched upstream files (expensive — each edit must be justified):

| File | Change |
|---|---|
| `server/parser/src/main/antlr4/.../CobolLexer.g4` | Reverts to pristine 2.5.1 (Task 4). |
| `server/parser/src/main/antlr4/.../CobolParser.g4` | Dialect delta reduced to 3 lines (Task 4). |
| `server/engine/src/main/antlr4/.../CICSParser.g4` | `contextSuperClass` removed → pristine (Task 5). |
| `server/engine/src/main/antlr4/.../Db2SqlParser.g4` | `contextSuperClass` removed → pristine (Task 5). |
| `server/dialect-idms/src/main/antlr4/.../IdmsParser.g4` | `contextSuperClass` removed (Task 5); `idmsStatements` reshape attempt (Task 8). |
| `server/engine/.../implicitDialects/cics/CICSDialect.java` | Loses `setDialectRecursively`, `addDialectTree`, poc imports (Task 5). |
| `server/engine/.../implicitDialects/sql/Db2SqlDialect.java` | Same, plus revert the unrelated import reorganisation (Task 5). |
| `server/dialect-idms/.../idms/IdmsDialect.java` | Loses all 18 insertions; gains a 1-line visitor swap (Tasks 5, 8). |
| `server/engine/.../implicitDialects/cics/CICSVisitor.java` | Reverts to pristine (Task 6). |
| `server/engine/.../implicitDialects/sql/Db2SqlVisitor.java` | Reverts to pristine (Task 7). |
| `server/dialect-idms/.../idms/IdmsVisitor.java` | Reverts to pristine, or keeps a ~6-line patch (Task 8). |

### Parent (`cobol-rekt/`)

| File | Change |
|---|---|
| `smojol-core/src/main/java/org/smojol/common/idms/DialectIntegratorListener.java` | Grafts by position instead of guid (Task 3). |
| `smojol-core/src/main/java/org/smojol/common/dialect/LanguageDialect.java` | Null-dialect predicate becomes `!isCovered(...)` (Task 3). |
| `smojol-core/src/main/java/org/smojol/common/ast/NodeText.java` | `dialectOriginalText` looks up by position (Task 3). |
| `smojol-toolkit/src/main/java/org/smojol/toolkit/ast/CompositeCobolFlowNode.java` | `isNullDialectNode` uses position (Task 3). |
| `smojol-toolkit/src/main/java/org/smojol/toolkit/analysis/pipeline/ParsePipeline.java` | Calls `PersistentData.reset()` at parse entry (Task 3). |
| `smojol-core/src/test/java/org/smojol/common/idms/DialectIntegratorListenerMissingKeyTest.java` | Rewritten against the positional API (Task 3). |
| `COBOL-LSP-INTEGRATION.md` | §2/§3/§5 updated (Task 9). |

---

### Task 1: Positional fragment registry in `PersistentData`

**Repository:** submodule.

This task is purely additive — the existing guid API stays so every downstream file keeps compiling. Task 5 removes the old API once nothing reads it.

**Files:**
- Modify: `che-che4z-lsp-for-cobol-integration/server/common/src/main/java/org/eclipse/lsp/cobol/common/poc/PersistentData.java`
- Test: `che-che4z-lsp-for-cobol-integration/server/common/src/test/java/org/eclipse/lsp/cobol/common/poc/PersistentDataTest.java` (create)

**Interfaces:**
- Consumes: nothing.
- Produces, all `public static` on `org.eclipse.lsp.cobol.common.poc.PersistentData`:
  - `class Fragment` with `public final int startLine, startChar, endLine`, `public final LocalisedDialect dialect`, `public final ParseTree tree`, and `public boolean covers(int line, int charPos)`
  - `void record(ParserRuleContext ctx, LocalisedDialect dialect)`
  - `Fragment fragmentAt(int line, int charPos)` — non-consuming lookup, returns `null` if none
  - `boolean isCovered(int line, int charPos)`
  - `Fragment claim(int line, int charPos)` — consuming lookup, returns `null` if none unclaimed
  - `int fragmentCount()`
  - `void reset()` — existing method, extended to clear the new state

- [ ] **Step 1: Write the failing test**

Create `server/common/src/test/java/org/eclipse/lsp/cobol/common/poc/PersistentDataTest.java`:

```java
package org.eclipse.lsp.cobol.common.poc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.antlr.v4.runtime.CommonToken;
import org.antlr.v4.runtime.ParserRuleContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Unit tests for the positional fragment registry. These manipulate static state, so the class is
 * pinned to a single thread — this module's surefire config sets {@code <parallel>all</parallel>}.
 */
@Execution(ExecutionMode.SAME_THREAD)
class PersistentDataTest {

  @BeforeEach
  void reset() {
    PersistentData.reset();
  }

  /** Builds a context whose start/stop tokens sit at the given coordinates. */
  private static ParserRuleContext contextAt(
      int startLine, int startChar, int endLine, int endChar) {
    ParserRuleContext ctx = new ParserRuleContext();
    CommonToken start = new CommonToken(1, "start");
    start.setLine(startLine);
    start.setCharPositionInLine(startChar);
    CommonToken stop = new CommonToken(1, "stop");
    stop.setLine(endLine);
    stop.setCharPositionInLine(endChar);
    ctx.start = start;
    ctx.stop = stop;
    return ctx;
  }

  @Test
  void recordStoresFragmentCoordinatesAndTree() {
    ParserRuleContext ctx = contextAt(12, 11, 13, 20);
    PersistentData.record(ctx, LocalisedDialect.IDMS);

    assertEquals(1, PersistentData.fragmentCount());
    PersistentData.Fragment fragment = PersistentData.fragmentAt(12, 11);
    assertEquals(12, fragment.startLine);
    assertEquals(11, fragment.startChar);
    assertEquals(13, fragment.endLine);
    assertEquals(LocalisedDialect.IDMS, fragment.dialect);
    assertSame(ctx, fragment.tree);
  }

  @Test
  void coversAcceptsPositionsAtOrAfterStartOnStartLine() {
    PersistentData.record(contextAt(12, 11, 13, 20), LocalisedDialect.IDMS);

    assertTrue(PersistentData.isCovered(12, 11), "start position must be covered");
    assertTrue(
        PersistentData.isCovered(12, 16),
        "a position 5 columns right of start must be covered — this is the IDMS _IF_ prefix case");
    assertFalse(PersistentData.isCovered(12, 10), "a position left of start must not be covered");
  }

  @Test
  void coversSpansToEndLineAndRejectsBeyond() {
    PersistentData.record(contextAt(12, 11, 13, 20), LocalisedDialect.IDMS);

    assertTrue(PersistentData.isCovered(13, 0), "any column on a later line must be covered");
    assertFalse(PersistentData.isCovered(14, 0), "a line past endLine must not be covered");
    assertFalse(PersistentData.isCovered(11, 99), "a line before startLine must not be covered");
  }

  @Test
  void claimConsumesFragmentSoItIsGraftedAtMostOnce() {
    PersistentData.record(contextAt(5, 0, 5, 10), LocalisedDialect.CICS);

    assertNotNull(PersistentData.claim(5, 0), "first claim must succeed");
    assertNull(PersistentData.claim(5, 0), "a claimed fragment must not be returned twice");
  }

  @Test
  void claimReturnsTheEarliestUnclaimedCoveringFragment() {
    ParserRuleContext first = contextAt(5, 0, 5, 10);
    ParserRuleContext second = contextAt(5, 0, 5, 10);
    PersistentData.record(first, LocalisedDialect.CICS);
    PersistentData.record(second, LocalisedDialect.CICS);

    assertSame(first, PersistentData.claim(5, 0).tree);
    assertSame(second, PersistentData.claim(5, 0).tree);
    assertNull(PersistentData.claim(5, 0));
  }

  @Test
  void isCoveredIgnoresWhetherFragmentWasClaimed() {
    PersistentData.record(contextAt(5, 0, 5, 10), LocalisedDialect.DB2_SQL);
    PersistentData.claim(5, 0);

    assertTrue(
        PersistentData.isCovered(5, 0),
        "isCovered must stay true after claim — it answers 'was this region a dialect fragment?'");
  }

  @Test
  void resetClearsFragmentsAndClaims() {
    PersistentData.record(contextAt(5, 0, 5, 10), LocalisedDialect.IDMS);
    PersistentData.claim(5, 0);

    PersistentData.reset();

    assertEquals(0, PersistentData.fragmentCount());
    PersistentData.record(contextAt(5, 0, 5, 10), LocalisedDialect.IDMS);
    assertEquals(
        1,
        PersistentData.fragmentCount(),
        "after reset a positionally identical fragment must be recordable and claimable again");
    assertNotNull(PersistentData.claim(5, 0), "claimed set must have been cleared by reset");
  }

  @Test
  void recordToleratesMissingStopToken() {
    ParserRuleContext ctx = contextAt(7, 4, 7, 9);
    ctx.stop = null;

    PersistentData.record(ctx, LocalisedDialect.IDMS);

    PersistentData.Fragment fragment = PersistentData.fragmentAt(7, 4);
    assertEquals(
        7, fragment.endLine, "with no stop token endLine must fall back to the start token's line");
  }

  @Test
  void lookupOnEmptyRegistryReturnsNullNotAnException() {
    assertNull(PersistentData.fragmentAt(1, 0));
    assertNull(PersistentData.claim(1, 0));
    assertFalse(PersistentData.isCovered(1, 0));
  }
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl common test \
  -Dtest=PersistentDataTest -DfailIfNoTests=false -Dsurefire.failIfNoSpecifiedTests=false
```

Expected: compilation failure — `cannot find symbol: method record(ParserRuleContext,LocalisedDialect)`, `class Fragment`, `fragmentAt`, `isCovered`, `claim`, `fragmentCount`.

- [ ] **Step 3: Add the positional API to `PersistentData`**

Add these imports to `PersistentData.java`:

```java
import org.antlr.v4.runtime.ParserRuleContext;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.Set;
```

Add this inside the class body, above the existing `counter` field. Leave every existing member in place.

```java
    /**
     * A dialect fragment that was blanked out of the extended document, together with the dialect
     * parse tree that produced it.
     *
     * <p>Coordinates are ANTLR token coordinates: {@code startLine}/{@code endLine} are 1-based
     * ({@code Token.getLine()}), {@code startChar} is 0-based
     * ({@code Token.getCharPositionInLine()}). They are positions in the <em>extended
     * document</em>, which every dialect parses in full, so they are directly comparable with
     * positions from the later COBOL parse of the same document.
     */
    public static final class Fragment {
        public final int startLine;
        public final int startChar;
        public final int endLine;
        public final LocalisedDialect dialect;
        public final ParseTree tree;

        private Fragment(int startLine, int startChar, int endLine, LocalisedDialect dialect, ParseTree tree) {
            this.startLine = startLine;
            this.startChar = startChar;
            this.endLine = endLine;
            this.dialect = dialect;
            this.tree = tree;
        }

        /**
         * Whether the given position falls inside this fragment. The check is a range, not an
         * equality test, because IDMS's {@code _IF_ } prefix is the one substitution that is not
         * length-preserving: it shifts the filler run 5 columns to the right of the recorded start.
         */
        public boolean covers(int line, int charPos) {
            if (line < startLine || line > endLine) return false;
            if (line == startLine) return charPos >= startChar;
            return true;
        }
    }

    private static final List<Fragment> fragments = new ArrayList<>();
    private static final Set<Fragment> claimed = Collections.newSetFromMap(new IdentityHashMap<>());

    /** Records the region {@code ctx} occupies in the extended document, and its parse tree. */
    public static void record(ParserRuleContext ctx, LocalisedDialect dialect) {
        int startLine = ctx.getStart().getLine();
        int startChar = ctx.getStart().getCharPositionInLine();
        int endLine = ctx.getStop() != null ? ctx.getStop().getLine() : startLine;
        fragments.add(new Fragment(startLine, startChar, endLine, dialect, ctx));
    }

    /** Non-consuming lookup. Returns {@code null} when no fragment covers the position. */
    public static Fragment fragmentAt(int line, int charPos) {
        for (Fragment fragment : fragments) {
            if (fragment.covers(line, charPos)) return fragment;
        }
        return null;
    }

    /** Whether any fragment — claimed or not — covers the position. */
    public static boolean isCovered(int line, int charPos) {
        return fragmentAt(line, charPos) != null;
    }

    /**
     * Consuming lookup: returns the earliest unclaimed fragment covering the position and marks it
     * claimed, so a fragment is grafted at most once. Returns {@code null} when none is left.
     */
    public static Fragment claim(int line, int charPos) {
        for (Fragment fragment : fragments) {
            if (claimed.contains(fragment)) continue;
            if (fragment.covers(line, charPos)) {
                claimed.add(fragment);
                return fragment;
            }
        }
        return null;
    }

    /** Number of fragments recorded since the last {@link #reset()}. */
    public static int fragmentCount() {
        return fragments.size();
    }
```

Extend `reset()` to clear the new state:

```java
    public static void reset() {
        counter = 0;
        tree = null;
        trees.clear();
        fragments.clear();
        claimed.clear();
    }
```

- [ ] **Step 4: Run test to verify it passes**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl common test \
  -Dtest=PersistentDataTest -DfailIfNoTests=false -Dsurefire.failIfNoSpecifiedTests=false
```

Expected: PASS, 9 tests.

- [ ] **Step 5: Verify the rest of `common` still builds and passes**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl common test
```

Expected: PASS, no regressions.

- [ ] **Step 6: Commit (submodule)**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration
git add server/common/src/main/java/org/eclipse/lsp/cobol/common/poc/PersistentData.java \
        server/common/src/test/java/org/eclipse/lsp/cobol/common/poc/PersistentDataTest.java
git commit -m "feat: add positional fragment registry to PersistentData

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 2: Record fragments positionally, alongside the existing marker

**Repository:** submodule.

Still additive. The three fork visitors keep injecting `_DIALECT_ <guid>` exactly as today, and additionally call `PersistentData.record`. Nothing downstream changes yet, so every existing test must still pass unchanged. This is what lets Task 3 (parent) and Task 4 (marker removal) each land green on their own.

**Files:**
- Modify: `che-che4z-lsp-for-cobol-integration/server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/cics/CicsSubstitutingVisitor.java:232`
- Modify: `che-che4z-lsp-for-cobol-integration/server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/sql/Db2SqlSubstitutingVisitor.java:501`
- Modify: `che-che4z-lsp-for-cobol-integration/server/dialect-idms/src/main/java/org/eclipse/lsp/cobol/dialects/idms/IdmsVisitor.java:263`
- Test: `che-che4z-lsp-for-cobol-integration/server/dialect-idms/src/test/java/org/eclipse/lsp/cobol/dialects/idms/usecases/TestPersistentDataExtraction.java` (add tests, do not yet rewrite the existing ones)

**Interfaces:**
- Consumes: `PersistentData.record(ParserRuleContext, LocalisedDialect)`, `PersistentData.fragmentCount()`, `PersistentData.fragmentAt(int, int)` from Task 1.
- Produces: for every substituted dialect fragment, exactly one recorded `Fragment` carrying the correct `LocalisedDialect`.

- [ ] **Step 1: Write the failing test**

Append to `TestPersistentDataExtraction`, and add `import org.eclipse.lsp.cobol.common.poc.PersistentData.Fragment;`:

```java
  // ---------- positional fragment recording ----------

  @Test
  void singleFinishStatementRecordsOnePositionalFragment() {
    String source = BOILERPLATE + "            FINISH.\n";
    analyze(source);

    assertEquals(1, PersistentData.fragmentCount(),
        "Expected exactly one recorded fragment for a single FINISH statement");
  }

  @Test
  void recordedFragmentCarriesIdmsDialectAndAParseTree() {
    String source = BOILERPLATE + "            FINISH.\n";
    analyze(source);

    // FINISH sits on line 6 of BOILERPLATE + one statement, indented 12 columns.
    Fragment fragment = PersistentData.fragmentAt(6, 12);
    assertNotNull(fragment, "A fragment must cover the FINISH statement at 6:12");
    assertEquals(LocalisedDialect.IDMS, fragment.dialect);
    assertNotNull(fragment.tree, "The recorded fragment must carry the IDMS parse tree");
    assertTrue(fragment.tree.getText().toUpperCase().contains("FINISH"),
        "The recorded tree must be the IDMS statement, got: " + fragment.tree.getText());
  }

  @Test
  void threeIdmsStatementsRecordThreeFragments() {
    String source =
        BOILERPLATE
            + "            BIND RUN-UNIT.\n"
            + "            READY.\n"
            + "            FINISH.\n";
    analyze(source);

    assertEquals(3, PersistentData.fragmentCount(),
        "Expected three recorded fragments for BIND + READY + FINISH");
  }

  @Test
  void fragmentCountMatchesExtractionCountSoBothMechanismsAgree() {
    String source =
        BOILERPLATE
            + "            BIND RUN-UNIT.\n"
            + "            READY.\n"
            + "            FINISH.\n";
    analyze(source);

    assertEquals(PersistentData.counter, PersistentData.fragmentCount(),
        "Every marker-injecting substitution must also record a positional fragment");
  }
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn install -pl common -DskipTests
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl dialect-idms -am test \
  -Dtest=TestPersistentDataExtraction -DfailIfNoTests=false -Dsurefire.failIfNoSpecifiedTests=false
```

Expected: the four new tests FAIL with `expected: <1> but was: <0>` (and `assertNotNull` failures), because nothing calls `record` yet. The 12 pre-existing tests must PASS.

If `recordedFragmentCarriesIdmsDialectAndAParseTree` fails on the *coordinates* rather than on null, print the actual fragment coordinates and correct the expected line/column in the test — `BOILERPLATE` is 5 lines, so the first appended statement is line 6, and the indentation is 12 spaces. Do not change `covers`.

- [ ] **Step 3: Record in `IdmsVisitor.replaceWithMetadata`**

In `server/dialect-idms/.../idms/IdmsVisitor.java`, change `replaceWithMetadata(AnnotatedParserRuleContext, String)` (line 263) to record before substituting:

```java
    private void replaceWithMetadata(AnnotatedParserRuleContext ctx, String staticPrefix) {
        PersistentData.record(ctx, LocalisedDialect.IDMS);
        String contextTextReference = PersistentData.next();
        ctx.getCustomData().put("IDMS-" + contextTextReference, new Object());
        ctx.getCustomData().put("DIALECT", "IDMS");
        String terminator = ".".equals(ctx.stop.getText()) ? "" : ".";
        addReplacementContext(ctx, String.format("%s_DIALECT_ %s %s", staticPrefix, contextTextReference, terminator));
        extractions++;
    }
```

Add `import org.eclipse.lsp.cobol.common.poc.LocalisedDialect;`.

Recording before substituting is required, not stylistic: after `addReplacementContext` runs, `ctx`'s tokens still hold their original coordinates (the range comes from the parse tree, not the document), but recording first keeps the ordering obvious and matches the reparented visitors in Tasks 6–8.

Then **remove the recording from `visitSchemaSection`** so schema sections do not produce a second fragment nested inside the `idmsSections` fragment (Correction 3). Change:

```java
  @Override
  public List<Node> visitSchemaSection(SchemaSectionContext ctx) {
    replaceWithMetadata(ctx, SCHEMA_SECTION + " ");
    return addTreeNode(ctx, locality -> new SectionNode(locality, SectionType.SCHEMA));
  }
```

to:

```java
  @Override
  public List<Node> visitSchemaSection(SchemaSectionContext ctx) {
    // No substitution here: schemaSection is a direct child of idmsSections, which
    // visitIdmsSections has already substituted and recorded. Substituting again would
    // create a second fragment with the same start position.
    return addTreeNode(ctx, locality -> new SectionNode(locality, SectionType.SCHEMA));
  }
```

Delete the now-unused `SCHEMA_SECTION` constant (line 53).

- [ ] **Step 4: Record in the CICS and DB2 substituting visitors**

In `server/engine/.../implicitDialects/cics/CicsSubstitutingVisitor.java`, add `PersistentData.record(ctx, LocalisedDialect.CICS);` as the first statement of `replaceWithMetadata(AnnotatedParserRuleContext, String)` (line 232), and add `import org.eclipse.lsp.cobol.common.poc.LocalisedDialect;`.

In `server/engine/.../implicitDialects/sql/Db2SqlSubstitutingVisitor.java`, add `PersistentData.record(ctx, LocalisedDialect.DB2_SQL);` as the first statement of `replaceWithMetadata(AnnotatedParserRuleContext, String)` (line 501), and add the same import.

Note that `Db2SqlSubstitutingVisitor.replaceWithMetadata` currently writes `ctx.getCustomData().put("DIALECT", "IDMS")` — a copy-paste bug. Leave it alone; the whole method dies in Task 7.

- [ ] **Step 5: Run test to verify it passes**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn install -pl common -DskipTests
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl dialect-idms -am test \
  -Dtest=TestPersistentDataExtraction -DfailIfNoTests=false -Dsurefire.failIfNoSpecifiedTests=false
```

Expected: PASS, 16 tests.

If `singleFinishStatementRecordsOnePositionalFragment` reports 2 instead of 1, a nested substitution is still recording twice — find it by logging each `record` call's coordinates, and suppress the inner one the same way `visitSchemaSection` was suppressed.

- [ ] **Step 6: Run the full dialect-idms and engine suites**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl dialect-idms -am test
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl engine -am test
```

Expected: `dialect-idms` all green (419 parsing tests plus the 16 above); `engine` 5177 tests, 0 failures. Removing the `visitSchemaSection` substitution is the one behavioural change in this task — if an IDMS schema-section test fails, that is the signal, and the fix is to confirm `visitIdmsSections` covers the same region (log both fragments' coordinates) rather than to restore the second substitution.

- [ ] **Step 7: Commit (submodule)**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration
git add server/dialect-idms/src/main/java/org/eclipse/lsp/cobol/dialects/idms/IdmsVisitor.java \
        server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/cics/CicsSubstitutingVisitor.java \
        server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/sql/Db2SqlSubstitutingVisitor.java \
        server/dialect-idms/src/test/java/org/eclipse/lsp/cobol/dialects/idms/usecases/TestPersistentDataExtraction.java
git commit -m "feat: record dialect fragments positionally alongside guid markers

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 3: Switch the smojol readers from guid to position

**Repository:** parent (`cobol-rekt`). Depends on Task 2 being installed.

The marker is still emitted after this task; it is simply no longer read. That keeps the parent green independently of Task 4.

**Files:**
- Modify: `smojol-core/src/main/java/org/smojol/common/idms/DialectIntegratorListener.java`
- Modify: `smojol-core/src/main/java/org/smojol/common/dialect/LanguageDialect.java:33-57`
- Modify: `smojol-core/src/main/java/org/smojol/common/ast/NodeText.java:27-37`
- Modify: `smojol-toolkit/src/main/java/org/smojol/toolkit/ast/CompositeCobolFlowNode.java:64-68`
- Modify: `smojol-toolkit/src/main/java/org/smojol/toolkit/analysis/pipeline/ParsePipeline.java`
- Test: `smojol-core/src/test/java/org/smojol/common/idms/DialectIntegratorListenerMissingKeyTest.java` (rewrite)

**Interfaces:**
- Consumes: `PersistentData.claim(int, int)`, `PersistentData.isCovered(int, int)`, `PersistentData.fragmentAt(int, int)`, `PersistentData.Fragment`, `PersistentData.reset()`.
- Produces: `DialectIntegratorListener.getRestores()` keeps its existing `int` signature and meaning.

- [ ] **Step 1: Install the submodule so the parent compiles against Task 2**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn install -DskipTests
```

Expected: BUILD SUCCESS.

- [ ] **Step 2: Write the failing test**

Replace the whole body of `smojol-core/src/test/java/org/smojol/common/idms/DialectIntegratorListenerMissingKeyTest.java` with:

```java
package org.smojol.common.idms;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.antlr.v4.runtime.CommonToken;
import org.antlr.v4.runtime.ParserRuleContext;
import org.eclipse.lsp.cobol.common.poc.LocalisedDialect;
import org.eclipse.lsp.cobol.common.poc.PersistentData;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

/**
 * Unit tests for {@link DialectIntegratorListener} correlation against {@link PersistentData}.
 *
 * <p>When no recorded fragment covers a {@code dialectNodeFiller}'s position, the listener must
 * skip gracefully rather than throw. That happens legitimately: upstream blanks some regions
 * (for example IDMS {@code visitObtainLRStatement}) without the fork recording a fragment, so
 * unclaimable fillers are expected, not exceptional.
 */
class DialectIntegratorListenerMissingKeyTest {

  @BeforeEach
  void resetPersistentData() {
    PersistentData.reset();
  }

  private static CobolParser.DialectNodeFillerContext fillerAt(int line, int charPos) {
    CommonToken start = new CommonToken(1, "​");
    start.setLine(line);
    start.setCharPositionInLine(charPos);
    CobolParser.DialectNodeFillerContext ctx =
        Mockito.mock(CobolParser.DialectNodeFillerContext.class);
    Mockito.when(ctx.getStart()).thenReturn(start);
    return ctx;
  }

  private static ParserRuleContext dialectTreeAt(int line, int charPos) {
    ParserRuleContext ctx = new ParserRuleContext();
    CommonToken token = new CommonToken(1, "FINISH");
    token.setLine(line);
    token.setCharPositionInLine(charPos);
    ctx.start = token;
    ctx.stop = token;
    return ctx;
  }

  @Test
  void uncoveredPositionDoesNotThrow() {
    DialectIntegratorListener listener = new DialectIntegratorListener();

    assertDoesNotThrow(
        () -> listener.enterDialectNodeFiller(fillerAt(99, 0)),
        "enterDialectNodeFiller must not throw when no fragment covers the position");
  }

  @Test
  void uncoveredPositionProducesZeroRestores() {
    DialectIntegratorListener listener = new DialectIntegratorListener();

    listener.enterDialectNodeFiller(fillerAt(99, 0));

    assertEquals(
        0, listener.getRestores(), "No restore must be counted for an uncovered filler position");
  }

  @Test
  void nullStartTokenIsHandledGracefully() {
    DialectIntegratorListener listener = new DialectIntegratorListener();
    CobolParser.DialectNodeFillerContext ctx =
        Mockito.mock(CobolParser.DialectNodeFillerContext.class);
    Mockito.when(ctx.getStart()).thenReturn(null);

    assertDoesNotThrow(
        () -> listener.enterDialectNodeFiller(ctx), "A null start token must not throw");
    assertEquals(0, listener.getRestores(), "No restore must be counted for a null start token");
  }

  @Test
  void coveredPositionGraftsTheFragmentAndCountsOneRestore() {
    PersistentData.record(dialectTreeAt(12, 11), LocalisedDialect.IDMS);
    DialectIntegratorListener listener = new DialectIntegratorListener();

    listener.enterDialectNodeFiller(fillerAt(12, 11));

    assertEquals(1, listener.getRestores(), "A covered filler position must graft exactly once");
  }

  @Test
  void aFragmentIsGraftedAtMostOnceAcrossTwoFillers() {
    PersistentData.record(dialectTreeAt(12, 11), LocalisedDialect.IDMS);
    DialectIntegratorListener listener = new DialectIntegratorListener();

    listener.enterDialectNodeFiller(fillerAt(12, 11));
    listener.enterDialectNodeFiller(fillerAt(12, 11));

    assertEquals(
        1,
        listener.getRestores(),
        "claim() consumes the fragment, so a second filler at the same position must not graft");
  }

  @Test
  void positionRightOfStartOnTheStartLineStillGrafts() {
    PersistentData.record(dialectTreeAt(12, 11), LocalisedDialect.IDMS);
    DialectIntegratorListener listener = new DialectIntegratorListener();

    // IDMS prepends "_IF_ " (5 chars), shifting the filler run right of the recorded start.
    listener.enterDialectNodeFiller(fillerAt(12, 16));

    assertEquals(1, listener.getRestores(), "The _IF_ prefix must not break correlation");
  }
}
```

`ctx.addChild(...)` is called on a Mockito mock in the grafting tests; that is a no-op on a mock and does not throw, so `getRestores()` is the observable. Do not assert on the mock's children.

- [ ] **Step 3: Run test to verify it fails**

```bash
cd /Users/avisheksengupta/code/cobol-rekt
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl smojol-core -am test \
  -Dtest=DialectIntegratorListenerMissingKeyTest -DfailIfNoTests=false -Dsurefire.failIfNoSpecifiedTests=false
```

Expected: `coveredPositionGraftsTheFragmentAndCountsOneRestore`, `aFragmentIsGraftedAtMostOnceAcrossTwoFillers` and `positionRightOfStartOnTheStartLineStillGrafts` FAIL with `expected: <1> but was: <0>` — the listener still returns early on `ctx.dialectGuid() == null`, and a mock returns `null` for it.

- [ ] **Step 4: Rewrite `DialectIntegratorListener` to graft by position**

Replace the body of `smojol-core/src/main/java/org/smojol/common/idms/DialectIntegratorListener.java`:

```java
package org.smojol.common.idms;

import java.util.logging.Logger;
import lombok.Getter;
import org.antlr.v4.runtime.Token;
import org.eclipse.lsp.cobol.common.poc.PersistentData;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.core.CobolParserBaseListener;

/**
 * This is a visitor into the generated parse tree which re-integrates the dialect code fragments
 * which were removed at the time of parsing standard Cobol.
 *
 * <p>Correlation is positional: every dialect visitor blanks its fragment out of the extended
 * document length-preservingly, so a fragment's recorded start position is still the position of
 * the filler run the COBOL parser produced in its place.
 */
public class DialectIntegratorListener extends CobolParserBaseListener {
  private static final Logger LOGGER = Logger.getLogger(DialectIntegratorListener.class.getName());
  @Getter private int restores = 0;

  @Override
  public void enterDialectNodeFiller(CobolParser.DialectNodeFillerContext ctx) {
    super.enterDialectNodeFiller(ctx);
    Token start = ctx.getStart();
    if (start == null) return;
    PersistentData.Fragment fragment =
        PersistentData.claim(start.getLine(), start.getCharPositionInLine());
    if (fragment == null) {
      LOGGER.finer(
          String.format(
              "No unclaimed dialect fragment covers %d:%d; skipping reinjection",
              start.getLine(), start.getCharPositionInLine()));
      return;
    }
    LOGGER.finer(
        String.format(
            "Restoring %s fragment recorded at %d:%d: %s",
            fragment.dialect, fragment.startLine, fragment.startChar, fragment.tree.getText()));
    ctx.addChild(new DialectContainerNode(fragment.tree, ctx, fragment.dialect));
    restores++;
  }
}
```

The log level drops from `warning` to `finer` deliberately: an unclaimable filler is now the normal case for regions upstream blanks without the fork recording anything (IDMS `visitObtainLRStatement`, `visitEraseStoreModifyLrStatementsOptions`; CICS `visitCicsDfhValue`), so a warning would fire on every ordinary parse.

- [ ] **Step 5: Run test to verify it passes**

```bash
cd /Users/avisheksengupta/code/cobol-rekt
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl smojol-core -am test \
  -Dtest=DialectIntegratorListenerMissingKeyTest -DfailIfNoTests=false -Dsurefire.failIfNoSpecifiedTests=false
```

Expected: PASS, 6 tests.

- [ ] **Step 6: Switch `LanguageDialect`'s null-dialect predicate to position**

In `smojol-core/src/main/java/org/smojol/common/dialect/LanguageDialect.java`, replace the two `findAllByCondition` predicates and add a helper. The full replacement for `verifyNoNullDialectStatements` in the `IDMS` constant:

```java
    @Override
    public void verifyNoNullDialectStatements(
        ParserRuleContext tree, EntityNavigatorBuilder navigatorBuilder) {
      CobolEntityNavigator navigator = navigatorBuilder.navigator(tree);
      List<ParseTree> nullDialectStatements =
          navigator.findAllByCondition(
              n ->
                  n.getClass() == CobolParser.DialectStatementContext.class
                      && ((CobolParser.DialectStatementContext) n).dialectNodeFiller() != null
                      && !coveredByFragment(
                          ((CobolParser.DialectStatementContext) n).dialectNodeFiller()),
              tree);
      nullDialectStatements.forEach(
          n -> {
            boolean removed = ((ParserRuleContext) n.getParent()).children.remove(n);
            java.util.logging.Logger logger = Logger.getLogger(LanguageDialect.class.getName());
            logger.finer(removed ? "removed" : "not removed");
          });
      List<ParseTree> nullIdmsNodes =
          navigator.findAllByCondition(
              n ->
                  n.getClass() == CobolParser.DialectNodeFillerContext.class
                      && !coveredByFragment((ParserRuleContext) n),
              tree);

      if (!nullIdmsNodes.isEmpty())
        throw new RuntimeException("Null IDMS nodes detected, please run preprocess()");
    }
```

And add, at enum level after the `dialect(String)` method:

```java
  /**
   * Whether a filler context sits inside a region that a dialect visitor recorded as a fragment.
   * Replaces the old test for the absence of a {@code _DIALECT_ <guid>} marker.
   */
  private static boolean coveredByFragment(ParserRuleContext ctx) {
    Token start = ctx.getStart();
    return start != null
        && PersistentData.isCovered(start.getLine(), start.getCharPositionInLine());
  }
```

Add imports:

```java
import org.antlr.v4.runtime.Token;
import org.eclipse.lsp.cobol.common.poc.PersistentData;
```

`isCovered` is used rather than `claim` on purpose: this runs *before* the integrator walk and must not consume anything.

- [ ] **Step 7: Switch `CompositeCobolFlowNode.isNullDialectNode` to position**

In `smojol-toolkit/src/main/java/org/smojol/toolkit/ast/CompositeCobolFlowNode.java`, replace:

```java
  private boolean isNullDialectNode(FlowNode node) {
    ParseTree n = node.getExecutionContext();
    return n.getClass() == CobolParser.DialectNodeFillerContext.class
        && ((CobolParser.DialectNodeFillerContext) n).whatever() != null;
  }
```

with:

```java
  private boolean isNullDialectNode(FlowNode node) {
    ParseTree n = node.getExecutionContext();
    if (n.getClass() != CobolParser.DialectNodeFillerContext.class) return false;
    Token start = ((CobolParser.DialectNodeFillerContext) n).getStart();
    return start == null
        || !PersistentData.isCovered(start.getLine(), start.getCharPositionInLine());
  }
```

Add imports:

```java
import org.antlr.v4.runtime.Token;
import org.eclipse.lsp.cobol.common.poc.PersistentData;
```

- [ ] **Step 8: Switch `NodeText.dialectOriginalText` to position**

In `smojol-core/src/main/java/org/smojol/common/ast/NodeText.java`, replace:

```java
  public static String dialectOriginalText(ParseTree astNode, FlowNodeService nodeService) {
    CobolEntityNavigator navigator = nodeService.getNavigator();
    ParseTree dialectGuidContext =
        navigator.findByCondition(
            astNode, t -> t.getClass() == CobolParser.DialectGuidContext.class);
    if (dialectGuidContext == null) return astNode.getText();
    String guid = dialectGuidContext.getText();

    ParseTree idmsTextNode = PersistentData.getDialectNode("IDMS-" + guid);
    return NodeText.originalText(idmsTextNode, NodeText::PASSTHROUGH);
  }
```

with:

```java
  public static String dialectOriginalText(ParseTree astNode, FlowNodeService nodeService) {
    CobolEntityNavigator navigator = nodeService.getNavigator();
    ParseTree filler =
        navigator.findByCondition(
            astNode, t -> t.getClass() == CobolParser.DialectNodeFillerContext.class);
    if (filler == null) return astNode.getText();
    Token start = ((CobolParser.DialectNodeFillerContext) filler).getStart();
    if (start == null) return astNode.getText();
    PersistentData.Fragment fragment =
        PersistentData.fragmentAt(start.getLine(), start.getCharPositionInLine());
    if (fragment == null) return astNode.getText();
    return NodeText.originalText(fragment.tree, NodeText::PASSTHROUGH);
  }
```

Add `import org.antlr.v4.runtime.Token;`. `fragmentAt` is used, not `claim`: this is a read for display and runs after grafting has already consumed the fragment.

- [ ] **Step 9: Reset `PersistentData` at parse entry**

In `smojol-toolkit/src/main/java/org/smojol/toolkit/analysis/pipeline/ParsePipeline.java`, insert a reset immediately before the `cleanUpCode` call:

```java
    // Fragment state is static (see COBOL-LSP-INTEGRATION.md section 5). Reset it per parse so
    // sequential parses cannot claim fragments recorded by the previous one.
    PersistentData.reset();
    ResultWithErrors<ExtendedText> resultWithErrors = preprocessor.cleanUpCode(documentUri, text);
```

Add `import org.eclipse.lsp.cobol.common.poc.PersistentData;`.

- [ ] **Step 10: Run the parent gates**

```bash
cd /Users/avisheksengupta/code/cobol-rekt
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl smojol-core -am test
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl smojol-toolkit -am test \
  -Dtest=IdmsDialectIntegrationTest -DfailIfNoTests=false -Dsurefire.failIfNoSpecifiedTests=false
```

Expected: `smojol-core` 108+ tests, 0 failures. `IdmsDialectIntegrationTest` 9/9 — this is the load-bearing suite: it asserts the exact four-node count, `LocalisedDialect.IDMS` annotation, marker-free node text, `DialectNodeFillerContext` parentage, one child per filler, and stability across sequential parses.

If `dialectContainerNodeCountMatchesIdmsStatementCount` reports fewer than 4, the marker-free correlation is losing a fragment. Diagnose by logging every `record` call's coordinates in the visitor and every `claim` call's coordinates in the listener, and compare — do not adjust the expected count.

- [ ] **Step 11: Commit (parent)**

```bash
cd /Users/avisheksengupta/code/cobol-rekt
git add smojol-core/src/main/java/org/smojol/common/idms/DialectIntegratorListener.java \
        smojol-core/src/main/java/org/smojol/common/dialect/LanguageDialect.java \
        smojol-core/src/main/java/org/smojol/common/ast/NodeText.java \
        smojol-toolkit/src/main/java/org/smojol/toolkit/ast/CompositeCobolFlowNode.java \
        smojol-toolkit/src/main/java/org/smojol/toolkit/analysis/pipeline/ParsePipeline.java \
        smojol-core/src/test/java/org/smojol/common/idms/DialectIntegratorListenerMissingKeyTest.java
git commit -m "refactor: correlate dialect fragments by position instead of guid

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 4: Stop injecting the marker and reduce the grammars

**Repository:** submodule (then verify the parent).

This is the task that actually removes the marker. It is atomic across the visitors and all four COBOL grammars, because a marker in the document with no lexer token would fail to lex, and a grammar rule referencing a removed token would fail to generate.

**Files:**
- Modify: `che-che4z-lsp-for-cobol-integration/server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolLexer.g4`
- Modify: `che-che4z-lsp-for-cobol-integration/server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolParser.g4:463, 1023, 1389, 2411, 2474`
- Modify: `che-che4z-lsp-for-cobol-integration/server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolSentenceParser.g4:754, 2112, 2172`
- Modify: `che-che4z-lsp-for-cobol-integration/server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolExpressionParser.g4:134, 145`
- Modify: `che-che4z-lsp-for-cobol-integration/server/engine/.../cics/CicsSubstitutingVisitor.java:232`
- Modify: `che-che4z-lsp-for-cobol-integration/server/engine/.../sql/Db2SqlSubstitutingVisitor.java:501`
- Modify: `che-che4z-lsp-for-cobol-integration/server/dialect-idms/.../idms/IdmsVisitor.java:263`
- Test: `che-che4z-lsp-for-cobol-integration/server/dialect-idms/src/test/.../usecases/TestPersistentDataExtraction.java` (rewrite the 12 guid tests)

**Interfaces:**
- Consumes: everything from Tasks 1–2.
- Produces: an extended document containing only length-preserving filler where a dialect fragment was, and a `CobolParser` whose `dialectNodeFiller` rule is `ZERO_WIDTH_SPACE+ DOT_FS? EOF?` with no `dialectGuid`, `whatever` or `eater` sub-rules.

- [ ] **Step 1: Rewrite the 12 guid-based tests in `TestPersistentDataExtraction`**

Delete these test methods and the class javadoc's guid description:

`extractedNodeIsRetrievableFromPersistentData`, `extractedNodeHasIdmsDialectAnnotation`, `allExtractedNodesAreRetrievable`, `allExtractedNodesHaveIdmsDialect`, `resetClearsTreesListBetweenAnalysisCalls`, `idmsIfConditionNodeIsRetrievableFromPersistentData`.

Change these to assert on `PersistentData.fragmentCount()` instead of `PersistentData.counter`, keeping their existing sources and expected numbers:

`singleFinishStatementProducesOneExtraction` (1), `threeIdmsStatementsProduceThreeExtractions` (3), `onlyIdmsStatementsAreExtractedNotCobol` (2), `idmsIfEmptyConditionProducesExactlyOneExtraction` (1), `idmsIfMemberConditionProducesExactlyOneExtraction` (1), `inquireMapIfProducesExactlyOneExtraction` (1), `idmsIfConditionAndInquireMapIfTogetherProduceTwoExtractions` (2).

Also delete the now-meaningless `fragmentCountMatchesExtractionCountSoBothMechanismsAgree` test added in Task 2, and replace `resetClearsTreesListBetweenAnalysisCalls` with:

```java
  @Test
  void resetClearsFragmentsBetweenAnalysisCalls() {
    String source = BOILERPLATE + "            FINISH.\n";

    analyze(source);
    int afterFirst = PersistentData.fragmentCount();
    assertTrue(afterFirst > 0, "After first analysis, at least one fragment must be recorded");

    PersistentData.reset();
    assertEquals(0, PersistentData.fragmentCount(), "After reset(), no fragments must remain");

    analyze(source);
    assertEquals(
        afterFirst,
        PersistentData.fragmentCount(),
        "A fresh analysis after reset() must record the same fragment count as the first");
  }
```

Update the class javadoc to describe positional recording rather than `"IDMS-N"` keys.

- [ ] **Step 2: Run test to verify it still passes before the mechanism changes**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl dialect-idms -am test \
  -Dtest=TestPersistentDataExtraction -DfailIfNoTests=false -Dsurefire.failIfNoSpecifiedTests=false
```

Expected: PASS. The rewritten tests are mechanism-agnostic, so they must pass both before and after the marker removal. That is the point — they become the regression net for Steps 3–6.

- [ ] **Step 3: Remove the marker prefix from all three visitors**

In `IdmsVisitor.java`, replace `replaceWithMetadata(AnnotatedParserRuleContext, String)` and its no-arg overload with a single method:

```java
    private void replaceWithMetadata(AnnotatedParserRuleContext ctx, String staticPrefix) {
        PersistentData.record(ctx, LocalisedDialect.IDMS);
        addReplacementContext(ctx, staticPrefix);
        extractions++;
    }
```

Delete the now-unused `replaceWithMetadata(AnnotatedParserRuleContext)` overload and update `visitIdmsSections` / `visitIdmsIfCondition` to call `replaceWithMetadata(ctx, "")`. Change `visitIdmsIfStatement`'s prefix from `IF + " "` to `IF` (the extra space was padding for the guid) and `visitIdmsStatements`' prefix stays `imperativeStatementCallOf(ctx) == null ? "" : IF`.

In `CicsSubstitutingVisitor.java`, replace `replaceWithMetadata(AnnotatedParserRuleContext, String)` with:

```java
    private void replaceWithMetadata(AnnotatedParserRuleContext ctx, String staticPrefix) {
        PersistentData.record(ctx, LocalisedDialect.CICS);
        addReplacementContext(ctx, staticPrefix);
        extractions++;
    }
```

In `Db2SqlSubstitutingVisitor.java`, the same with `LocalisedDialect.DB2_SQL`.

All three now write only length-preserving filler (plus IDMS's `_IF_` prefix). The `customData` puts, the `PersistentData.next()` calls and the `_DIALECT_`/terminator string building are all gone.

- [ ] **Step 4: Reduce `CobolParser.g4` to the three-line dialect delta**

Five edits. Each `-` line is the current fork text and each `+` line is the replacement.

`dialectSection` (line 463) — revert to pristine:

```antlr
 dialectSection
-   : dialectSectionBlock | dialectNodeFiller
+   : dialectNodeFiller
    ;
```

`dialectStatement` (line 1023) — drop the bare-token alternative:

```antlr
 dialectStatement
-   : ZERO_WIDTH_SPACE | dialectIfStatment | dialectNodeFiller
+   : dialectIfStatment | dialectNodeFiller
    ;
```

**This reorder is load-bearing and non-obvious.** Upstream's rule leads with the bare `ZERO_WIDTH_SPACE` alternative, so a blanked procedure-division statement matches a bare token and produces no rule context to graft onto. Letting `dialectNodeFiller` absorb the token is what makes the marker-free design work at all — during the spike, 1 of 4 fragments correlated before this change and 4 of 4 after.

Delete `dialectSectionBlock` (lines 1389-1391) entirely:

```antlr
-dialectSectionBlock
-   : DIALECT_SCHEMA_SECTION dialectNodeFiller*
-   ;
-
```

Delete `dialectGuid` (line 2411):

```antlr
-dialectGuid: integerLiteral;
 dialectLiteral: dialectNodeFiller+ DOT_FS?;
```

Collapse `dialectNodeFiller`/`whatever`/`eater` and the commented-out corpse (lines 2474 onward):

```antlr
 dialectNodeFiller
-    : (DIALECT_MARKER dialectGuid DOT_FS? eater) | whatever
-    ;
-
-whatever: eater;
-// NEWLINE should probably be removed
-eater
     : ZERO_WIDTH_SPACE+ DOT_FS? EOF?
     ;
 
 dot_fs
     : DOT_FS
     ;
-
-//dialectNodeFiller
-//    : ZERO_WIDTH_SPACE+
-//    ;
```

Leave `conditionalStatementCall : statement | dialectStatement` (line 856) exactly as it is — it is one of the three surviving lines. Leave every other fork change in this file alone: the smojol named-rule extraction (`divisor`, `multiplyLhs`, `charBasedOperator`, `additionalCondition`, `multipleArithmeticExpressions`/`additionalArithmeticExpression`, `nextSentence`/`nextSentenceWrapperStatement`, `mapStatement`, `genericOnClauseStatement`/`onClauseBlock`) and the genuine COBOL fixes (`ALTERNATE (RECORD KEY? | KEY)`, `level88DataUsageClause`, `(GENERIC_COMMENTS NONNUMERICLITERAL)?`, `| ZERO`) are all out of scope.

- [ ] **Step 5: Clean the two fork-added grammars**

`CobolSentenceParser.g4` and `CobolExpressionParser.g4` are fork-added files that both declare `options {tokenVocab = CobolLexer;}` and are generated on every build, so they must lose the same rules or the build breaks once the tokens go.

In `CobolSentenceParser.g4`: delete `dialectGuid: integerLiteral;` (line 2112), delete `whatever: eater;` and the `eater` rule (lines 2176-2182 including the commented `//dialectNodeFiller` block), and collapse `dialectNodeFiller` (line 2172) to `: ZERO_WIDTH_SPACE+ DOT_FS? EOF?`. Also reorder `dialectStatement` (line 754) from `: dialectNodeFiller | dialectIfStatment` — it is already filler-first here, so leave it.

In `CobolExpressionParser.g4`: delete `dialectGuid: integerLiteral;` (line 134), delete `whatever: eater;` and `eater` (lines 149-150), and collapse `dialectNodeFiller` (line 145) to `: ZERO_WIDTH_SPACE+ DOT_FS? EOF?`.

- [ ] **Step 6: Revert `CobolLexer.g4` to pristine**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration
git checkout 2.5.1 -- server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolLexer.g4
git diff --numstat 2.5.1 -- server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolLexer.g4
```

Expected: no output from the second command — the file is byte-identical to 2.5.1. This removes `DIALECT_MARKER_LITERAL`, `SCHEMA`, `DIALECT_SCHEMA_SECTION` and `DIALECT_MARKER`, and also two whitespace-only fork edits.

- [ ] **Step 7: Verify grammar generation is clean**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl parser generate-sources 2>&1 | grep -i "error\|warning.*implicit\|DIALECT_MARKER\|SCHEMA"
```

Expected: no output. Any `implicit token definition` warning or `reference to undefined rule` error names a leftover in one of the four grammars — fix that grammar, do not re-add the token.

- [ ] **Step 8: Run the submodule gates**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn install -DskipTests
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl dialect-idms -am test
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl engine -am test
```

Expected: `dialect-idms` all green including the rewritten `TestPersistentDataExtraction`; `engine` 5177 tests, 0 failures.

- [ ] **Step 9: Run the parent gates**

```bash
cd /Users/avisheksengupta/code/cobol-rekt
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl smojol-core -am test
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl smojol-toolkit -am test \
  -Dtest=IdmsDialectIntegrationTest -DfailIfNoTests=false -Dsurefire.failIfNoSpecifiedTests=false
```

Expected: `smojol-core` green; `IdmsDialectIntegrationTest` 9/9. This is the moment the whole marker-free design is proven end to end. `reinjectedNodesReturnNonEmptyOriginalText` asserts marker-free node text and will now be trivially satisfied; `eachDialectContainerNodeHasExactlyOneChild` and `dialectContainerNodeCountMatchesIdmsStatementCount` are the real signals.

- [ ] **Step 10: Commit (submodule, then parent has nothing to commit)**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration
git add server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolLexer.g4 \
        server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolParser.g4 \
        server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolSentenceParser.g4 \
        server/parser/src/main/antlr4/org/eclipse/lsp/cobol/core/CobolExpressionParser.g4 \
        server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/cics/CicsSubstitutingVisitor.java \
        server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/sql/Db2SqlSubstitutingVisitor.java \
        server/dialect-idms/src/main/java/org/eclipse/lsp/cobol/dialects/idms/IdmsVisitor.java \
        server/dialect-idms/src/test/java/org/eclipse/lsp/cobol/dialects/idms/usecases/TestPersistentDataExtraction.java
git commit -m "refactor: drop the _DIALECT_ marker and reduce the dialect grammar delta

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 5: Remove the guid API, `contextSuperClass`, and `setDialectRecursively`

**Repository:** submodule.

Everything removed here has no readers left after Task 4. This is where the metric moves: three grammars and three dialect classes shed dialect-substitution surface.

**Files:**
- Modify: `che-che4z-lsp-for-cobol-integration/server/common/.../poc/PersistentData.java`
- Delete: `che-che4z-lsp-for-cobol-integration/server/common/.../poc/AnnotatedParserRuleContext.java`
- Modify: `che-che4z-lsp-for-cobol-integration/server/engine/src/main/antlr4/.../cics/CICSParser.g4:16` (options line)
- Modify: `che-che4z-lsp-for-cobol-integration/server/engine/src/main/antlr4/.../sql/Db2SqlParser.g4` (options line)
- Modify: `che-che4z-lsp-for-cobol-integration/server/dialect-idms/src/main/antlr4/.../idms/IdmsParser.g4:16`
- Modify: `che-che4z-lsp-for-cobol-integration/server/engine/.../cics/CICSDialect.java`
- Modify: `che-che4z-lsp-for-cobol-integration/server/engine/.../sql/Db2SqlDialect.java`
- Modify: `che-che4z-lsp-for-cobol-integration/server/dialect-idms/.../idms/IdmsDialect.java`
- Modify: the three substituting visitors, whose `replaceWithMetadata` parameter type changes from `AnnotatedParserRuleContext` to `ParserRuleContext`

**Interfaces:**
- Consumes: the positional API from Task 1.
- Produces: `PersistentData` with only `Fragment`, `record`, `fragmentAt`, `isCovered`, `claim`, `fragmentCount`, `reset`. `AnnotatedParserRuleContext` no longer exists.

- [ ] **Step 1: Write the failing test**

Add to `server/common/src/test/java/org/eclipse/lsp/cobol/common/poc/PersistentDataTest.java`:

```java
  @Test
  void persistentDataExposesOnlyThePositionalApi() {
    java.util.Set<String> methodNames = new java.util.TreeSet<>();
    for (java.lang.reflect.Method m : PersistentData.class.getDeclaredMethods()) {
      if (m.isSynthetic()) continue;
      methodNames.add(m.getName());
    }

    assertEquals(
        new java.util.TreeSet<>(
            java.util.Arrays.asList(
                "claim", "fragmentAt", "fragmentCount", "isCovered", "record", "reset")),
        methodNames,
        "The guid-keyed API (next/addDialectTree/getDialectNode/dialect/treeCount) must be gone");
    assertEquals(
        0,
        PersistentData.class.getDeclaredFields().length
            - java.util.Arrays.stream(PersistentData.class.getDeclaredFields())
                .filter(f -> f.getName().equals("fragments") || f.getName().equals("claimed"))
                .count(),
        "Only the fragments list and claimed set may remain as static state");
  }
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl common test \
  -Dtest=PersistentDataTest -DfailIfNoTests=false -Dsurefire.failIfNoSpecifiedTests=false
```

Expected: FAIL — the actual method set still contains `next`, `addDialectTree`, `getDialectNode`, `dialect`, `treeCount`.

- [ ] **Step 3: Strip `PersistentData` to the positional API**

Delete `counter`, `next()`, `tree`, `trees`, `addDialectTree`, both `getDialectNode` overloads, `dialect(String)`, `treeCount()`, and the `TerminalNode`/`Objects` imports. Reduce `reset()` to:

```java
    /** Clears all fragment state. Must be called at parse entry — see ParsePipeline. */
    public static void reset() {
        fragments.clear();
        claimed.clear();
    }
```

Update the class javadoc's "counter, trees list" wording to "fragments list, claimed set".

- [ ] **Step 4: Run test to verify it passes**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl common test \
  -Dtest=PersistentDataTest -DfailIfNoTests=false -Dsurefire.failIfNoSpecifiedTests=false
```

Expected: PASS, 10 tests.

- [ ] **Step 5: Remove `contextSuperClass` from the three dialect grammars**

`server/engine/src/main/antlr4/org/eclipse/lsp/cobol/implicitDialects/cics/CICSParser.g4`:

```antlr
-options {tokenVocab = CICSLexer; superClass = MessageServiceParser; contextSuperClass = org.eclipse.lsp.cobol.common.poc.AnnotatedParserRuleContext;}
+options {tokenVocab = CICSLexer; superClass = MessageServiceParser;}
```

`server/engine/src/main/antlr4/org/eclipse/lsp/cobol/implicitDialects/sql/Db2SqlParser.g4` — the same single-line edit with `tokenVocab = Db2SqlLexer`. Verify the exact current text with `git diff 2.5.1 -- <path>` first; that file's fork patch is exactly +1/−1.

`server/dialect-idms/src/main/antlr4/org/eclipse/lsp/cobol/dialects/idms/IdmsParser.g4:16`:

```antlr
-options {tokenVocab = IdmsLexer;  superClass = MessageServiceParser; contextSuperClass = org.eclipse.lsp.cobol.common.poc.AnnotatedParserRuleContext;}
+options {tokenVocab = IdmsLexer;  superClass = MessageServiceParser;}
```

- [ ] **Step 6: Change the visitors' `replaceWithMetadata` parameter type**

With `contextSuperClass` gone, generated contexts extend `ParserRuleContext`. In all three substituting visitors change `replaceWithMetadata(AnnotatedParserRuleContext ctx, String staticPrefix)` to `replaceWithMetadata(ParserRuleContext ctx, String staticPrefix)`, and change any private `addReplacementContext(AnnotatedParserRuleContext ...)` helpers in the fork copies to `ParserRuleContext` too. Remove every `import org.eclipse.lsp.cobol.common.poc.AnnotatedParserRuleContext;`.

In `Db2SqlSubstitutingVisitor`'s private `addReplacementContext`, the call `constructRange(node)` takes a `TerminalNode`; leave it alone.

- [ ] **Step 7: Strip the three dialect classes**

`CICSDialect.java` — revert every one of the +28/−4 insertions except the visitor seam. Delete: the `ParseTree`/`TerminalNode` imports, the three `poc` imports, the `setDialectRecursively` method and its call, and the `PersistentData.addDialectTree(result)` call. Delete the dead `//    CICSVisitor cicsVisitor = new CICSVisitor(context, messageService, 0);` line. **Keep**: the `visitorBuilder` field, the extra 3-arg constructor, and the `visitorBuilder.visitor(...)` line in `processText` — something must choose between the original and substituting visitors, and this is the cheapest place.

`Db2SqlDialect.java` — the same deletions, plus revert the ~10 lines of unrelated import reorganisation:

```java
-import org.eclipse.lsp.cobol.common.copybook.CopybookModel;
-import org.eclipse.lsp.cobol.common.copybook.CopybookName;
-import org.eclipse.lsp.cobol.common.copybook.CopybookService;
-import org.eclipse.lsp.cobol.common.copybook.SQLBackend;
+import org.eclipse.lsp.cobol.common.copybook.*;
```

and drop the five explicit `java.util.*` imports the fork expanded. Verify against `git show 2.5.1:server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/sql/Db2SqlDialect.java`. Keep the `visitorBuilder` field, both constructors, and the `visitorBuilder.visitor(...)` line.

`IdmsDialect.java` — delete all 18 insertions: the `ParseTree`/`TerminalNode` imports, the three `poc` imports, the `System.out.println("[INFO] Extracted " + visitor.getExtractions() + " nodes.")` line, the `PersistentData.addDialectTree(startRuleContext)` line, and `setDialectRecursively` plus its call. Task 8 adds the one-line visitor swap at line 227; until then `new IdmsVisitor(context)` stays and IDMS keeps substituting via the still-patched `IdmsVisitor`.

- [ ] **Step 8: Delete `AnnotatedParserRuleContext`**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration
git rm server/common/src/main/java/org/eclipse/lsp/cobol/common/poc/AnnotatedParserRuleContext.java
grep -rn "AnnotatedParserRuleContext" --include='*.java' --include='*.g4' server/ | grep -v target
```

Expected: the `grep` produces no output.

- [ ] **Step 9: Run all gates**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn install -DskipTests
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl dialect-idms -am test
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl engine -am test
cd /Users/avisheksengupta/code/cobol-rekt
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl smojol-core -am test
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl smojol-toolkit -am test \
  -Dtest=IdmsDialectIntegrationTest -DfailIfNoTests=false -Dsurefire.failIfNoSpecifiedTests=false
```

Expected: all green. `engine` 5177/0, `IdmsDialectIntegrationTest` 9/9.

- [ ] **Step 10: Measure and record the metric**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration
git diff --numstat --diff-filter=M 2.5.1 -- 'server/**/src/main/**' | sort -rn
git diff --numstat --diff-filter=M 2.5.1 -- 'server/**/src/main/**' | awk '{a+=$1;d+=$2;n++} END {print n" files, +"a"/-"d" = "a+d" lines"}'
```

Expected: `CobolLexer.g4`, `CICSParser.g4` and `Db2SqlParser.g4` have dropped off the list entirely; the total is around 16 files / ~420 lines. Record the exact numbers in the commit message.

- [ ] **Step 11: Commit (submodule)**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration
git add -A server/
git commit -m "refactor: drop AnnotatedParserRuleContext and the guid registry API

Reverts CICSParser.g4 and Db2SqlParser.g4 to pristine 2.5.1 and removes
setDialectRecursively from all three dialect classes.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 6: Reparent `CicsSubstitutingVisitor` onto `CICSVisitor`

**Repository:** submodule.

The equivalence test comes first, and it is the point of this task as much as the reparent is: `DialectService:212` gates substitution on `config.isAddCicsPlaceholder()`, so all 5177 engine tests run the ORIGINAL visitor and the substituting path has no che4z coverage at all. That is why the copy could drift and silently lose `CICSOptionsCheckUtility`, `cics_abend`/`ExecCicsAbendNode`, the `missingEndExec` diagnostic and `visitVariableNameUsage`.

**Files:**
- Test: `che-che4z-lsp-for-cobol-integration/server/engine/src/test/java/org/eclipse/lsp/cobol/implicitDialects/cics/CicsSubstitutingVisitorEquivalenceTest.java` (create)
- Rewrite: `che-che4z-lsp-for-cobol-integration/server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/cics/CicsSubstitutingVisitor.java`
- Delete: `che-che4z-lsp-for-cobol-integration/server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/cics/ErrorHandlingCICSVisitor.java`
- Modify: `che-che4z-lsp-for-cobol-integration/server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/cics/CICSVisitor.java:74, 89`
- Modify: `che-che4z-lsp-for-cobol-integration/server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/cics/CICSVisitorBuilder.java`
- Modify: `che-che4z-lsp-for-cobol-integration/server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/cics/CICSDialect.java` (local variable type)

**Interfaces:**
- Consumes: `PersistentData.record`, `PersistentData.fragmentCount`, `LocalisedDialect.CICS`.
- Produces: `CicsSubstitutingVisitor extends CICSVisitor` with a package-private constructor `CicsSubstitutingVisitor(DialectProcessingContext, MessageService)`; `CICSVisitorBuilder.visitor(...)` returns `CICSVisitor`.

- [ ] **Step 1: Write the failing test**

Create `server/engine/src/test/java/org/eclipse/lsp/cobol/implicitDialects/cics/CicsSubstitutingVisitorEquivalenceTest.java`. The test lives in the `cics` package so it can see the package-private visitors and `CICSVisitorBuilder`.

```java
package org.eclipse.lsp.cobol.implicitDialects.cics;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.util.List;
import java.util.stream.Collectors;
import org.eclipse.lsp.cobol.common.copybook.CopybookService;
import org.eclipse.lsp.cobol.common.dialects.CobolLanguageId;
import org.eclipse.lsp.cobol.common.dialects.DialectOutcome;
import org.eclipse.lsp.cobol.common.dialects.DialectProcessingContext;
import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.eclipse.lsp.cobol.common.mapping.ExtendedDocument;
import org.eclipse.lsp.cobol.common.message.MessageService;
import org.eclipse.lsp.cobol.common.model.tree.Node;
import org.eclipse.lsp.cobol.common.poc.PersistentData;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Closes the coverage gap that let {@code CicsSubstitutingVisitor} drift away from
 * {@link CICSVisitor}. {@code DialectService} selects the substituting visitor only when
 * {@code AnalysisConfig.isAddCicsPlaceholder()} is set, so the whole engine suite exercises the
 * original visitor and nothing exercised the fork's copy.
 *
 * <p>The contract asserted here is: the substituting visitor produces the same nodes and the same
 * errors as the original, and additionally records one positional fragment per substituted EXEC
 * CICS block.
 */
@Execution(ExecutionMode.SAME_THREAD)
class CicsSubstitutingVisitorEquivalenceTest {

  private static final String URI = "file:///cics.cbl";
  private static final String TEXT =
      "        IDENTIFICATION DIVISION.\n"
          + "        PROGRAM-ID. CICSTEST.\n"
          + "        DATA DIVISION.\n"
          + "        WORKING-STORAGE SECTION.\n"
          + "        01 WS-MSG PIC X(10).\n"
          + "        PROCEDURE DIVISION.\n"
          + "            EXEC CICS SEND TEXT FROM(WS-MSG) END-EXEC.\n";

  private CopybookService copybookService;
  private MessageService messageService;

  @BeforeEach
  void setUp() {
    PersistentData.reset();
    copybookService = mock(CopybookService.class, withSettings().lenient());
    messageService = mock(MessageService.class, withSettings().lenient());
    when(messageService.getMessage(anyString())).thenReturn("message");
    when(messageService.getMessage(anyString(), any(Object[].class))).thenReturn("message");
  }

  private static DialectProcessingContext freshContext() {
    DialectProcessingContext context =
        DialectProcessingContext.builder()
            .extendedDocument(new ExtendedDocument(TEXT, URI))
            .programDocumentUri(URI)
            .languageId(CobolLanguageId.COBOL.getId())
            .build();
    context.getExtendedDocument().commitTransformations();
    return context;
  }

  private DialectOutcome run(CICSVisitorBuilder builder, DialectProcessingContext context) {
    return new CICSDialect(copybookService, messageService, builder)
        .processText(context)
        .getResult();
  }

  private List<SyntaxError> errorsOf(CICSVisitorBuilder builder, DialectProcessingContext context) {
    return new CICSDialect(copybookService, messageService, builder)
        .processText(context)
        .getErrors();
  }

  private static List<String> nodeShapes(List<Node> nodes) {
    return nodes.stream()
        .map(n -> n.getClass().getSimpleName() + "@" + n.getLocality().getRange())
        .sorted()
        .collect(Collectors.toList());
  }

  @Test
  void substitutingVisitorProducesTheSameNodesAsTheOriginal() {
    PersistentData.reset();
    List<Node> original = run(CICSVisitorBuilder.ORIGINAL, freshContext()).getDialectNodes();
    PersistentData.reset();
    List<Node> substituting =
        run(CICSVisitorBuilder.SUBSTITUTING, freshContext()).getDialectNodes();

    assertEquals(
        nodeShapes(original),
        nodeShapes(substituting),
        "Reparenting must not change which dialect nodes the CICS visitor produces");
  }

  @Test
  void substitutingVisitorProducesTheSameErrorsAsTheOriginal() {
    PersistentData.reset();
    List<String> original =
        errorsOf(CICSVisitorBuilder.ORIGINAL, freshContext()).stream()
            .map(SyntaxError::toString)
            .sorted()
            .collect(Collectors.toList());
    PersistentData.reset();
    List<String> substituting =
        errorsOf(CICSVisitorBuilder.SUBSTITUTING, freshContext()).stream()
            .map(SyntaxError::toString)
            .sorted()
            .collect(Collectors.toList());

    assertEquals(
        original,
        substituting,
        "Reparenting must not change which errors the CICS visitor reports");
  }

  @Test
  void substitutingVisitorBlanksTheExecBlockJustLikeTheOriginal() {
    PersistentData.reset();
    DialectProcessingContext originalContext = freshContext();
    run(CICSVisitorBuilder.ORIGINAL, originalContext);
    PersistentData.reset();
    DialectProcessingContext substitutingContext = freshContext();
    run(CICSVisitorBuilder.SUBSTITUTING, substitutingContext);

    assertEquals(
        originalContext.getExtendedDocument().toString(),
        substitutingContext.getExtendedDocument().toString(),
        "Substitution must stay length-preserving and marker-free, so both documents must match");
  }

  @Test
  void substitutingVisitorRecordsOneFragmentForTheExecBlock() {
    PersistentData.reset();
    run(CICSVisitorBuilder.SUBSTITUTING, freshContext());

    assertEquals(
        1,
        PersistentData.fragmentCount(),
        "One EXEC CICS block must record exactly one positional fragment");
    PersistentData.Fragment fragment = PersistentData.fragmentAt(7, 12);
    assertTrue(
        fragment != null && fragment.tree.getText().toUpperCase().contains("SEND"),
        "The recorded fragment must be the EXEC CICS parse tree");
  }

  @Test
  void originalVisitorRecordsNoFragments() {
    PersistentData.reset();
    run(CICSVisitorBuilder.ORIGINAL, freshContext());

    assertEquals(
        0,
        PersistentData.fragmentCount(),
        "The original visitor must not record fragments — smojol's flag is what turns this on");
  }
}
```

If the Mockito `MessageService` mock proves insufficient (an NPE or a `MissingResourceException` from inside `CICSOptionsCheckUtility`), swap it for the real service:

```java
LocaleStore localeStore = mock(LocaleStore.class);
when(localeStore.getApplicationLocale()).thenReturn(java.util.Locale.ENGLISH);
messageService =
    new PropertiesMessageService(
        "resourceBundles/messages", localeStore, mock(SettingsService.class),
        mock(WorkingFolderService.class), () -> mock(CobolLanguageClient.class));
```

`"resourceBundles/messages"` is the bundle `EngineModule` binds, and it lives in `engine/src/main/resources`.

- [ ] **Step 2: Run test to verify it fails**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl engine -am test \
  -Dtest=CicsSubstitutingVisitorEquivalenceTest -DfailIfNoTests=false -Dsurefire.failIfNoSpecifiedTests=false
```

Expected: at least `substitutingVisitorProducesTheSameNodesAsTheOriginal` and `substitutingVisitorProducesTheSameErrorsAsTheOriginal` FAIL. The copy is missing `CICSOptionsCheckUtility`, `cics_abend`/`ExecCicsAbendNode`, `missingEndExec` and `visitVariableNameUsage`, so both node sets and error sets diverge. **Record the actual diff from the failure output in the commit message** — it is the measured cost of the drift and the justification for this task.

`substitutingVisitorBlanksTheExecBlockJustLikeTheOriginal` should already pass after Task 4.

- [ ] **Step 3: Reparent `CicsSubstitutingVisitor`**

Replace the entire 364-line file with:

```java
/*
 * Copyright (c) 2023 Broadcom.
 * The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *    Broadcom, Inc. - initial API and implementation
 *
 */
package org.eclipse.lsp.cobol.implicitDialects.cics;

import java.util.List;
import org.eclipse.lsp.cobol.common.dialects.DialectProcessingContext;
import org.eclipse.lsp.cobol.common.message.MessageService;
import org.eclipse.lsp.cobol.common.model.tree.Node;
import org.eclipse.lsp.cobol.common.poc.LocalisedDialect;
import org.eclipse.lsp.cobol.common.poc.PersistentData;

/**
 * A {@link CICSVisitor} that additionally records the extended-document region each CICS fragment
 * occupied, together with its parse tree, so smojol can graft the CICS subtree back onto the COBOL
 * parse tree afterwards.
 *
 * <p>Substitution itself is entirely upstream's: {@code visitCicsExecBlock} calls
 * {@code changeContextToDialectStatement} and {@code visitCicsDfhResp} calls
 * {@code addReplacementContext}, both length-preserving. This class only records and delegates, so
 * it inherits every upstream improvement instead of shadowing it.
 *
 * <p>Recording happens before the {@code super} call, which is safe: the range comes from the parse
 * tree, not from the document, so it is unaffected by the blanking {@code super} performs.
 *
 * <p>{@code visitCicsDfhValue} is deliberately not overridden. Upstream substitutes there and the
 * fork never recorded a fragment for it, so leaving it alone preserves existing behaviour.
 */
class CicsSubstitutingVisitor extends CICSVisitor {

  CicsSubstitutingVisitor(DialectProcessingContext context, MessageService messageService) {
    super(context, messageService);
  }

  @Override
  public List<Node> visitCicsExecBlock(CICSParser.CicsExecBlockContext ctx) {
    PersistentData.record(ctx, LocalisedDialect.CICS);
    return super.visitCicsExecBlock(ctx);
  }

  @Override
  public List<Node> visitCicsDfhResp(CICSParser.CicsDfhRespContext ctx) {
    PersistentData.record(ctx, LocalisedDialect.CICS);
    return super.visitCicsDfhResp(ctx);
  }
}
```

No `protected` widening is needed anywhere: the subclass never calls upstream's private helpers, it just lets them run.

- [ ] **Step 4: Revert `CICSVisitor` to pristine and delete `ErrorHandlingCICSVisitor`**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration
git checkout 2.5.1 -- server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/cics/CICSVisitor.java
git rm server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/cics/ErrorHandlingCICSVisitor.java
git diff --numstat 2.5.1 -- server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/cics/CICSVisitor.java
```

Expected: the `git diff --numstat` produces no output. That restores `class CICSVisitor extends CICSParserBaseVisitor<List<Node>>` and removes the field shadowing between `CICSVisitor.errors` and `ErrorHandlingCICSVisitor.errors`.

- [ ] **Step 5: Retype the builder and the dialect's local variable**

`CICSVisitorBuilder.java`:

```java
-    ErrorHandlingCICSVisitor visitor(DialectProcessingContext context, MessageService messageService);
+    CICSVisitor visitor(DialectProcessingContext context, MessageService messageService);
```

`CICSDialect.processText`:

```java
-      ErrorHandlingCICSVisitor cicsVisitor = visitorBuilder.visitor(context, messageService);
+      CICSVisitor cicsVisitor = visitorBuilder.visitor(context, messageService);
```

- [ ] **Step 6: Run test to verify it passes**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl engine -am test \
  -Dtest=CicsSubstitutingVisitorEquivalenceTest -DfailIfNoTests=false -Dsurefire.failIfNoSpecifiedTests=false
```

Expected: PASS, 5 tests. Node and error equivalence now hold by construction.

- [ ] **Step 7: Run all gates and triage new CICS diagnostics**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn install -DskipTests
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl engine -am test
cd /Users/avisheksengupta/code/cobol-rekt
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl smojol-toolkit -am test
```

Expected: `engine` 5177/0. Reparenting activates upstream code the fork's substituting path has never run — `CICSOptionsCheckUtility` option validation, `cics_abend` handling, the `missingEndExec` diagnostic — so smojol fixtures containing CICS may surface new errors. That is the point of the change. Triage each one as a real CICS defect in the fixture or a genuine upstream false positive; do not suppress them by re-adding the copy.

- [ ] **Step 8: Commit (submodule)**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration
git add -A server/engine/
git commit -m "refactor: reparent CicsSubstitutingVisitor onto CICSVisitor

Reverts CICSVisitor.java to pristine 2.5.1 and deletes ErrorHandlingCICSVisitor.
Adds an equivalence test that flips isAddCicsPlaceholder, closing the coverage
gap that let the copy drift away from upstream.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 7: Reparent `Db2SqlSubstitutingVisitor` onto `Db2SqlVisitor`

**Repository:** submodule. Structurally identical to Task 6, with three overrides instead of two.

**Files:**
- Test: `che-che4z-lsp-for-cobol-integration/server/engine/src/test/java/org/eclipse/lsp/cobol/implicitDialects/sql/Db2SqlSubstitutingVisitorEquivalenceTest.java` (create)
- Rewrite: `che-che4z-lsp-for-cobol-integration/server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/sql/Db2SqlSubstitutingVisitor.java`
- Delete: `che-che4z-lsp-for-cobol-integration/server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/cics/MarkerDb2SqlVisitor.java`
- Modify: `che-che4z-lsp-for-cobol-integration/server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/sql/Db2SqlVisitor.java:58, 67`
- Modify: `che-che4z-lsp-for-cobol-integration/server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/sql/Db2SqlVisitorBuilder.java`
- Modify: `che-che4z-lsp-for-cobol-integration/server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/sql/Db2SqlDialect.java`

**Interfaces:**
- Consumes: `PersistentData.record`, `LocalisedDialect.DB2_SQL`.
- Produces: `Db2SqlSubstitutingVisitor extends Db2SqlVisitor` with a package-private constructor `(DialectProcessingContext, MessageService, CopybookService, boolean)`; `Db2SqlVisitorBuilder.visitor(...)` returns `Db2SqlVisitor`.

- [ ] **Step 1: Write the failing test**

Create `server/engine/src/test/java/org/eclipse/lsp/cobol/implicitDialects/sql/Db2SqlSubstitutingVisitorEquivalenceTest.java`:

```java
package org.eclipse.lsp.cobol.implicitDialects.sql;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.util.List;
import java.util.stream.Collectors;
import org.eclipse.lsp.cobol.common.copybook.CopybookService;
import org.eclipse.lsp.cobol.common.dialects.CobolLanguageId;
import org.eclipse.lsp.cobol.common.dialects.DialectProcessingContext;
import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.eclipse.lsp.cobol.common.mapping.ExtendedDocument;
import org.eclipse.lsp.cobol.common.message.MessageService;
import org.eclipse.lsp.cobol.common.model.tree.Node;
import org.eclipse.lsp.cobol.common.poc.PersistentData;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Closes the coverage gap that let {@code Db2SqlSubstitutingVisitor} drift away from
 * {@link Db2SqlVisitor}. {@code DialectService} selects the substituting visitor only when
 * {@code AnalysisConfig.isAddDb2SqlPlaceholder()} is set, so the whole engine suite exercises the
 * original visitor and nothing exercised the fork's copy.
 *
 * <p>The contract asserted here is: the substituting visitor produces the same nodes and the same
 * errors as the original, and additionally records one positional fragment per substituted EXEC SQL
 * block.
 */
@Execution(ExecutionMode.SAME_THREAD)
class Db2SqlSubstitutingVisitorEquivalenceTest {

  private static final String URI = "file:///db2.cbl";
  private static final String TEXT =
      "        IDENTIFICATION DIVISION.\n"
          + "        PROGRAM-ID. DB2TEST.\n"
          + "        DATA DIVISION.\n"
          + "        WORKING-STORAGE SECTION.\n"
          + "        01 WS-MSG PIC X(10).\n"
          + "        PROCEDURE DIVISION.\n"
          + "            EXEC SQL SELECT 1 INTO :WS-MSG FROM SYSIBM.SYSDUMMY1 END-EXEC.\n";

  private CopybookService copybookService;
  private MessageService messageService;

  @BeforeEach
  void setUp() {
    PersistentData.reset();
    copybookService = mock(CopybookService.class, withSettings().lenient());
    messageService = mock(MessageService.class, withSettings().lenient());
    when(messageService.getMessage(anyString())).thenReturn("message");
    when(messageService.getMessage(anyString(), any(Object[].class))).thenReturn("message");
  }

  private static DialectProcessingContext freshContext() {
    DialectProcessingContext context =
        DialectProcessingContext.builder()
            .extendedDocument(new ExtendedDocument(TEXT, URI))
            .programDocumentUri(URI)
            .languageId(CobolLanguageId.COBOL.getId())
            .build();
    context.getExtendedDocument().commitTransformations();
    return context;
  }

  private DialectOutcome run(Db2SqlVisitorBuilder builder, DialectProcessingContext context) {
    return new Db2SqlDialect(copybookService, messageService, builder)
        .processText(context)
        .getResult();
  }

  private List<SyntaxError> errorsOf(
      Db2SqlVisitorBuilder builder, DialectProcessingContext context) {
    return new Db2SqlDialect(copybookService, messageService, builder)
        .processText(context)
        .getErrors();
  }

  private static List<String> nodeShapes(List<Node> nodes) {
    return nodes.stream()
        .map(n -> n.getClass().getSimpleName() + "@" + n.getLocality().getRange())
        .sorted()
        .collect(Collectors.toList());
  }

  @Test
  void substitutingVisitorProducesTheSameNodesAsTheOriginal() {
    PersistentData.reset();
    List<Node> original = run(Db2SqlVisitorBuilder.ORIGINAL, freshContext()).getDialectNodes();
    PersistentData.reset();
    List<Node> substituting =
        run(Db2SqlVisitorBuilder.SUBSTITUTING, freshContext()).getDialectNodes();

    assertEquals(
        nodeShapes(original),
        nodeShapes(substituting),
        "Reparenting must not change which dialect nodes the DB2 visitor produces");
  }

  @Test
  void substitutingVisitorProducesTheSameErrorsAsTheOriginal() {
    PersistentData.reset();
    List<String> original =
        errorsOf(Db2SqlVisitorBuilder.ORIGINAL, freshContext()).stream()
            .map(SyntaxError::toString)
            .sorted()
            .collect(Collectors.toList());
    PersistentData.reset();
    List<String> substituting =
        errorsOf(Db2SqlVisitorBuilder.SUBSTITUTING, freshContext()).stream()
            .map(SyntaxError::toString)
            .sorted()
            .collect(Collectors.toList());

    assertEquals(
        original, substituting, "Reparenting must not change which errors the DB2 visitor reports");
  }

  @Test
  void substitutingVisitorBlanksTheExecBlockJustLikeTheOriginal() {
    PersistentData.reset();
    DialectProcessingContext originalContext = freshContext();
    run(Db2SqlVisitorBuilder.ORIGINAL, originalContext);
    PersistentData.reset();
    DialectProcessingContext substitutingContext = freshContext();
    run(Db2SqlVisitorBuilder.SUBSTITUTING, substitutingContext);

    assertEquals(
        originalContext.getExtendedDocument().toString(),
        substitutingContext.getExtendedDocument().toString(),
        "Substitution must stay length-preserving and marker-free, so both documents must match");
  }

  @Test
  void substitutingVisitorRecordsOneFragmentForTheExecBlock() {
    PersistentData.reset();
    run(Db2SqlVisitorBuilder.SUBSTITUTING, freshContext());

    assertEquals(
        1,
        PersistentData.fragmentCount(),
        "One EXEC SQL block must record exactly one positional fragment");
    PersistentData.Fragment fragment = PersistentData.fragmentAt(7, 12);
    assertTrue(
        fragment != null && fragment.tree.getText().toUpperCase().contains("SELECT"),
        "The recorded fragment must be the EXEC SQL parse tree");
  }

  @Test
  void originalVisitorRecordsNoFragments() {
    PersistentData.reset();
    run(Db2SqlVisitorBuilder.ORIGINAL, freshContext());

    assertEquals(
        0,
        PersistentData.fragmentCount(),
        "The original visitor must not record fragments — smojol's flag is what turns this on");
  }
}
```

Two things may need adjusting once it runs:

- `DialectOutcome` needs an import; add `org.eclipse.lsp.cobol.common.dialects.DialectOutcome` if the compiler asks for it (it is in the same package as `DialectProcessingContext`).
- `copybookService` must return something usable for `PredefinedCopybooks`/`SQLBackend` lookups. The lenient mock returns `null` for everything; if `processText` NPEs on a copybook model, stub the exact call the stack trace names to return `new CopybookModel(null, null, null, null)` or an empty `Optional`, whichever its signature requires. If `Db2SqlDialect.processText` needs SQL processing enabled and reads it from the context's config, add `.config(AnalysisConfig.substitutingDefaultConfig(CopybookProcessingMode.ENABLED))` to the builder — but only if `DialectProcessingContext.builder()` actually exposes a `config` setter; otherwise the default applies and no change is needed.

If the Mockito `MessageService` mock proves insufficient (an NPE or a `MissingResourceException`), swap it for the real service:

```java
LocaleStore localeStore = mock(LocaleStore.class);
when(localeStore.getApplicationLocale()).thenReturn(java.util.Locale.ENGLISH);
messageService =
    new PropertiesMessageService(
        "resourceBundles/messages", localeStore, mock(SettingsService.class),
        mock(WorkingFolderService.class), () -> mock(CobolLanguageClient.class));
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl engine -am test \
  -Dtest=Db2SqlSubstitutingVisitorEquivalenceTest -DfailIfNoTests=false -Dsurefire.failIfNoSpecifiedTests=false
```

Expected: the node and error equivalence tests FAIL, showing where the 510-line copy has drifted from the 470-line upstream visitor. Record the diff in the commit message.

- [ ] **Step 3: Reparent `Db2SqlSubstitutingVisitor`**

Replace the entire 510-line file with:

```java
/*
 * Copyright (c) 2023 Broadcom.
 * The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *    Broadcom, Inc. - initial API and implementation
 *
 */
package org.eclipse.lsp.cobol.implicitDialects.sql;

import java.util.List;
import org.eclipse.lsp.cobol.common.copybook.CopybookService;
import org.eclipse.lsp.cobol.common.dialects.DialectProcessingContext;
import org.eclipse.lsp.cobol.common.message.MessageService;
import org.eclipse.lsp.cobol.common.model.tree.Node;
import org.eclipse.lsp.cobol.common.poc.LocalisedDialect;
import org.eclipse.lsp.cobol.common.poc.PersistentData;

/**
 * A {@link Db2SqlVisitor} that additionally records the extended-document region each DB2 SQL
 * fragment occupied, together with its parse tree, so smojol can graft the SQL subtree back onto
 * the COBOL parse tree afterwards.
 *
 * <p>Substitution itself is entirely upstream's {@code addReplacementContext}, which is
 * length-preserving. This class only records and delegates.
 */
class Db2SqlSubstitutingVisitor extends Db2SqlVisitor {

  Db2SqlSubstitutingVisitor(
      DialectProcessingContext context,
      MessageService messageService,
      CopybookService copybookService,
      boolean isSqlProcessingEnabled) {
    super(context, messageService, copybookService, isSqlProcessingEnabled);
  }

  @Override
  public List<Node> visitExecRule(Db2SqlParser.ExecRuleContext ctx) {
    PersistentData.record(ctx, LocalisedDialect.DB2_SQL);
    return super.visitExecRule(ctx);
  }

  @Override
  public List<Node> visitLob_host_variables_arrays(
      Db2SqlParser.Lob_host_variables_arraysContext ctx) {
    PersistentData.record(ctx, LocalisedDialect.DB2_SQL);
    return super.visitLob_host_variables_arrays(ctx);
  }

  @Override
  public List<Node> visitBinary_host_variable(Db2SqlParser.Binary_host_variableContext ctx) {
    PersistentData.record(ctx, LocalisedDialect.DB2_SQL);
    return super.visitBinary_host_variable(ctx);
  }
}
```

Note that the old copy had a 3-arg constructor at `:80` with no callers anywhere in either repository (verified by grep). It is not reproduced.

- [ ] **Step 4: Revert `Db2SqlVisitor` to pristine and delete `MarkerDb2SqlVisitor`**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration
git checkout 2.5.1 -- server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/sql/Db2SqlVisitor.java
git rm server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/cics/MarkerDb2SqlVisitor.java
git diff --numstat 2.5.1 -- server/engine/src/main/java/org/eclipse/lsp/cobol/implicitDialects/sql/Db2SqlVisitor.java
```

Expected: no output from `git diff --numstat`. Pristine `Db2SqlVisitor` is `@AllArgsConstructor class Db2SqlVisitor extends Db2SqlParserBaseVisitor<List<Node>>`; Lombok's generated constructor is public and skips the initialised final `errors` field, so the subclass's `super(...)` call resolves. This also deletes a class that sat in the `cics` package while extending `Db2SqlParserBaseVisitor` from `sql`.

- [ ] **Step 5: Retype the builder and the dialect's local variable**

`Db2SqlVisitorBuilder.java`:

```java
-import org.eclipse.lsp.cobol.implicitDialects.cics.MarkerDb2SqlVisitor;
...
-    MarkerDb2SqlVisitor visitor(DialectProcessingContext context, MessageService messageService, CopybookService copybookService, boolean isSqlProcessingEnabled);
+    Db2SqlVisitor visitor(DialectProcessingContext context, MessageService messageService, CopybookService copybookService, boolean isSqlProcessingEnabled);
```

Also update the javadoc's `@return MarkerDb2SqlVisitor` to `@return Db2SqlVisitor`.

`Db2SqlDialect.processText`:

```java
-    MarkerDb2SqlVisitor db2SqlVisitor =
+    Db2SqlVisitor db2SqlVisitor =
         visitorBuilder.visitor(context, messageService, copybookService, isSqlProcessingEnabled);
```

and remove `import org.eclipse.lsp.cobol.implicitDialects.cics.MarkerDb2SqlVisitor;`.

- [ ] **Step 6: Run test to verify it passes**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl engine -am test \
  -Dtest=Db2SqlSubstitutingVisitorEquivalenceTest -DfailIfNoTests=false -Dsurefire.failIfNoSpecifiedTests=false
```

Expected: PASS, 5 tests.

- [ ] **Step 7: Run all gates**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn install -DskipTests
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl engine -am test
cd /Users/avisheksengupta/code/cobol-rekt
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl smojol-toolkit -am test
```

Expected: `engine` 5177/0; smojol-toolkit green.

- [ ] **Step 8: Commit (submodule)**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration
git add -A server/engine/
git commit -m "refactor: reparent Db2SqlSubstitutingVisitor onto Db2SqlVisitor

Reverts Db2SqlVisitor.java to pristine 2.5.1 and deletes MarkerDb2SqlVisitor.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 8: Reparent IDMS

**Repository:** submodule. This is the largest reparent and the only one with a grammar obstacle.

Two obstacles, both of which the executor must understand before touching anything:

**Obstacle A — the accessor.** Pristine `IdmsVisitor.visitIdmsStatements` reads `ctx.imperativeStatementCall()`. The fork restructured `idmsStatements : idmsOptTermStatement | idmsMandTermStatement`, so `IdmsStatementsContext.imperativeStatementCall()` no longer exists and reverting the visitor would fail to **compile**.

**Obstacle B — `IF 1 + 1 = 2`.** Upstream's `visitIdmsStatements` handles the ON-path-status case by calling `addReplacementImperativeStatementContext` (`IdmsVisitor.java:283`), which does `extendedDocument.clear(range)` on the region before the imperative statement and then writes the literal text `"IF 1 + 1 = 2"`. `clear` bottoms out in `ExtendedTextLine.clear(int,int)` at `common/src/main/java/org/eclipse/lsp/cobol/common/mapping/ExtendedTextLine.java:161`, which is `characters.subList(start, min(end, size)).forEach(c -> c.setCharacter(' '))` — **plain space, not `CobolDialect.FILLER`**. So that path produces no `ZERO_WIDTH_SPACE` run, hence no `dialectNodeFiller` context, hence no graft anchor: the IDMS fragment would be recorded but never claimed. The fork's `_IF_ ` prefix approach exists precisely to keep an anchor. So the IDMS subclass must *not* delegate to `super` for the ON-path-status case; it substitutes itself with `_IF_ ` and blanks with filler.

This was verified by reading `ExtendedTextLine`, not inferred — do not "simplify" `IdmsSubstitutingVisitor.visitIdmsStatements` into a plain `super` delegation.

That means the subclass needs its own blanking helper duplicating upstream's private `addReplacementContext(ctx, prefix)`. `IdmsSubstitutingVisitor.java` is a fork-*added* file, so that duplication costs nothing on the metric — unlike widening the upstream method to `protected`, which would cost a patched upstream line.

**Files:**
- Modify: `che-che4z-lsp-for-cobol-integration/server/dialect-idms/src/main/antlr4/org/eclipse/lsp/cobol/dialects/idms/IdmsParser.g4:150-160`
- Create: `che-che4z-lsp-for-cobol-integration/server/dialect-idms/src/main/java/org/eclipse/lsp/cobol/dialects/idms/IdmsSubstitutingVisitor.java`
- Modify: `che-che4z-lsp-for-cobol-integration/server/dialect-idms/src/main/java/org/eclipse/lsp/cobol/dialects/idms/IdmsVisitor.java`
- Modify: `che-che4z-lsp-for-cobol-integration/server/dialect-idms/src/main/java/org/eclipse/lsp/cobol/dialects/idms/IdmsDialect.java:227`

**Interfaces:**
- Consumes: `PersistentData.record`, `LocalisedDialect.IDMS`, and upstream `IdmsVisitor`'s public `visitIdmsStatements`, `visitIdmsSections`, `visitIdmsIfStatement`, `visitIdmsIfCondition`.
- Produces: `IdmsSubstitutingVisitor extends IdmsVisitor` with a package-private constructor `IdmsSubstitutingVisitor(DialectProcessingContext)` and a `getExtractions()` accessor (unused after Task 5 removed the `System.out.println`, so omit it).

- [ ] **Step 1: Attempt the `idmsStatements` reshape**

Replace lines 150-160 of `IdmsParser.g4`:

```antlr
 idmsStatements
-    : idmsOptTermStatement | idmsMandTermStatement
+    : idmsStatementBody imperativeStatementCall? idmsOnClause?
     ;
 
-idmsOptTermStatement
-    : idmsStmtsOptTermOn endClause? imperativeStatementCall? idmsOnClause?
-    ;
-
-idmsMandTermStatement
-    : idmsStmtsMandTermOn (SEMICOLON_FS idmsOnClause? | DOT_FS | imperativeStatementCall)
-    ;
+idmsStatementBody
+    : idmsStmtsOptTermOn endClause?
+    | idmsStmtsMandTermOn (SEMICOLON_FS | DOT_FS)?
+    ;
```

`imperativeStatementCall` now appears **exactly once** in `idmsStatements`, which is the hard constraint. Note the deliberate relaxation: the mandatory terminator on `idmsStmtsMandTermOn` (currently `transferStatement` only) becomes optional, because the original rule allowed `transferStatement imperativeStatementCall` with no terminator and there is no single-occurrence shape that expresses "terminator or imperative call, but exactly one". Step 3's test run is what decides whether that relaxation is acceptable.

- [ ] **Step 2: Verify the generated accessor is scalar, not a `List`**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl dialect-idms generate-sources
grep -n "imperativeStatementCall" \
  dialect-idms/target/generated-sources/antlr4/org/eclipse/lsp/cobol/dialects/idms/IdmsParser.java \
  | grep -A2 -B2 "class IdmsStatementsContext" || true
sed -n '/class IdmsStatementsContext/,/^\t}/p' \
  dialect-idms/target/generated-sources/antlr4/org/eclipse/lsp/cobol/dialects/idms/IdmsParser.java \
  | grep "imperativeStatementCall"
```

Expected exactly:

```java
		public ImperativeStatementCallContext imperativeStatementCall() {
```

**If the output is `public List<ImperativeStatementCallContext> imperativeStatementCall()`, stop and take the fallback in Step 8.** A `List` accessor compiles against upstream's `ctx.imperativeStatementCall() == null`, but the expression is then always false, so the visitor silently takes the wrong branch — strictly worse than a compile error. Verify against the generated parser; never by inspecting the grammar.

- [ ] **Step 3: Run the IDMS suite to check the relaxation**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl dialect-idms -am test
```

Expected: 419 parsing tests green, `TestPersistentDataExtraction` green. Any failure mentioning `TRANSFER` or a swallowed following statement means the mandatory-terminator relaxation regressed — take the fallback in Step 8.

- [ ] **Step 4: Write the failing test**

Add to `TestPersistentDataExtraction`:

```java
  @Test
  void onPathStatusStatementStillRecordsAFragmentAndKeepsAFillerAnchor() {
    String source =
        "        IDENTIFICATION DIVISION.\n"
            + "        PROGRAM-ID. ONPATH.\n"
            + "        DATA DIVISION.\n"
            + "        WORKING-STORAGE SECTION.\n"
            + "        01 WS-X PIC 9.\n"
            + "        PROCEDURE DIVISION.\n"
            + "            OBTAIN NEXT EMPLOYEE-RECORD WITHIN EMP-SET\n"
            + "                ON DB-END-OF-SET MOVE 1 TO WS-X.\n";
    analyze(source);

    assertTrue(PersistentData.fragmentCount() >= 1,
        "An ON path-status statement must still record a fragment");
  }

  @Test
  void schemaSectionRecordsExactlyOneFragmentNotTwo() {
    String source =
        "        IDENTIFICATION DIVISION.\n"
            + "        PROGRAM-ID. SCHEMATEST.\n"
            + "        ENVIRONMENT DIVISION.\n"
            + "        IDMS-CONTROL SECTION.\n"
            + "        PROTOCOL. MODE IS BATCH DEBUG.\n"
            + "        DATA DIVISION.\n"
            + "        SCHEMA SECTION.\n"
            + "        DB EMPSS01 WITHIN EMPSCHM VERSION 1.\n"
            + "        WORKING-STORAGE SECTION.\n"
            + "        PROCEDURE DIVISION.\n"
            + "            FINISH.\n";
    analyze(source);

    // idmsSections records the IDMS-CONTROL and SCHEMA sections; schemaSection must not record a
    // second, nested fragment at the same start position. Plus one for FINISH.
    assertEquals(3, PersistentData.fragmentCount(),
        "IDMS-CONTROL SECTION + SCHEMA SECTION + FINISH must record exactly 3 fragments; "
            + "4 means visitSchemaSection is double-recording inside the idmsSections fragment");
  }
```

Run them before the reparent. If `schemaSectionRecordsExactlyOneFragmentNotTwo` reports a number other than 3, adjust the *expected* count to whatever Task 2's `visitIdmsSections`-only recording actually produces for this source, and keep the assertion message — the invariant being locked in is "no nested duplicate at the same start position", which you verify by logging each fragment's coordinates and confirming no two share a start.

- [ ] **Step 5: Create `IdmsSubstitutingVisitor`**

```java
/*
 * Copyright (c) 2022 Broadcom.
 * The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *    Broadcom - initial API and implementation
 *
 */
package org.eclipse.lsp.cobol.dialects.idms;

import static org.eclipse.lsp.cobol.dialects.idms.IdmsParser.DOT_FS;

import java.util.List;
import org.antlr.v4.runtime.ParserRuleContext;
import org.eclipse.lsp.cobol.common.dialects.CobolDialect;
import org.eclipse.lsp.cobol.common.dialects.DialectProcessingContext;
import org.eclipse.lsp.cobol.common.model.tree.Node;
import org.eclipse.lsp.cobol.common.poc.LocalisedDialect;
import org.eclipse.lsp.cobol.common.poc.PersistentData;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser.IdmsIfConditionContext;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser.IdmsIfStatementContext;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser.IdmsSectionsContext;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser.IdmsStatementsContext;

/**
 * An {@link IdmsVisitor} that additionally records the extended-document region each IDMS fragment
 * occupied, together with its parse tree, so smojol can graft the IDMS subtree back onto the COBOL
 * parse tree afterwards.
 *
 * <p>Three of the four overrides record and delegate to {@code super}. The fourth,
 * {@link #visitIdmsStatements}, cannot: for an ON path-status statement upstream calls
 * {@code addReplacementImperativeStatementContext}, which uses
 * {@code ExtendedDocument.clear(range)} plus the literal text {@code "IF 1 + 1 = 2"}. {@code clear}
 * writes spaces rather than {@link CobolDialect#FILLER}, so that path leaves no filler run for the
 * COBOL parser to turn into a {@code dialectNodeFiller} — and therefore no anchor to graft onto.
 * This class substitutes that case itself, prefixing {@code _IF_ } so the COBOL parser matches
 * {@code dialectIfStatment} and the trailing imperative statement stays reachable.
 *
 * <p>{@code visitSchemaSection} is deliberately not overridden: {@code schemaSection} is a direct
 * child of {@code idmsSections}, which {@link #visitIdmsSections} already records, so recording it
 * again would create two fragments sharing a start position.
 */
class IdmsSubstitutingVisitor extends IdmsVisitor {
  private static final String IF = "_IF_ ";

  private final DialectProcessingContext context;

  IdmsSubstitutingVisitor(DialectProcessingContext context) {
    super(context);
    this.context = context;
  }

  @Override
  public List<Node> visitIdmsStatements(IdmsStatementsContext ctx) {
    PersistentData.record(ctx, LocalisedDialect.IDMS);
    if (ctx.imperativeStatementCall() == null) {
      return super.visitIdmsStatements(ctx);
    }
    // Do not delegate: upstream would write "IF 1 + 1 = 2" over a space-cleared region, leaving no
    // filler anchor. Blank with FILLER and prefix _IF_ instead.
    blankWithPrefix(ctx, IF);
    return visitChildren(ctx);
  }

  @Override
  public List<Node> visitIdmsSections(IdmsSectionsContext ctx) {
    PersistentData.record(ctx, LocalisedDialect.IDMS);
    return super.visitIdmsSections(ctx);
  }

  @Override
  public List<Node> visitIdmsIfStatement(IdmsIfStatementContext ctx) {
    PersistentData.record(ctx, LocalisedDialect.IDMS);
    return super.visitIdmsIfStatement(ctx);
  }

  @Override
  public List<Node> visitIdmsIfCondition(IdmsIfConditionContext ctx) {
    PersistentData.record(ctx, LocalisedDialect.IDMS);
    return super.visitIdmsIfCondition(ctx);
  }

  /**
   * Length-preserving blanking with a literal prefix. Mirrors upstream's private
   * {@code IdmsVisitor.addReplacementContext(ctx, prefix)}; duplicated here rather than widened to
   * {@code protected} because this file is fork-added and costs nothing, whereas widening would add
   * a patched line to an upstream file.
   */
  private void blankWithPrefix(ParserRuleContext ctx, String prefix) {
    String newText =
        prefix
            + context
                .getExtendedDocument()
                .toString()
                .substring(ctx.start.getStartIndex(), ctx.stop.getStopIndex() + 1)
                .replaceAll("[^ \n]", CobolDialect.FILLER);
    // Preserve a trailing dot: the COBOL parser does not expect dots to be consumed by the IDMS
    // preprocessor.
    if (ctx.getStop().getType() == DOT_FS) {
      newText = newText.substring(0, newText.length() - 1) + ".";
    }
    context.getExtendedDocument().replace(DialectUtils.constructRange(ctx), newText);
  }
}
```

`IdmsVisitor`'s `context` field is `private`, hence the second copy of the reference here. That is one field in a fork-added file, not an upstream patch.

- [ ] **Step 6: Revert `IdmsVisitor` to pristine**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration
git checkout 2.5.1 -- server/dialect-idms/src/main/java/org/eclipse/lsp/cobol/dialects/idms/IdmsVisitor.java
git diff --numstat 2.5.1 -- server/dialect-idms/src/main/java/org/eclipse/lsp/cobol/dialects/idms/IdmsVisitor.java
```

Expected: no output. This drops `replaceWithMetadata` (both overloads), `SCHEMA_SECTION`, the `extractions` counter, `imperativeStatementCallOf()`, the pure-delegation `visitIdmsStmtsOptTermOn`, and the `poc` imports. `IF = "_IF_ "` and `addReplacementContext(ctx, prefix)` come back as upstream's own code — the subclass declares its own `IF` because upstream's is `private`.

- [ ] **Step 7: Swap the visitor in `IdmsDialect`**

`IdmsDialect.java:227`:

```java
-    IdmsVisitor visitor = new IdmsVisitor(context);
+    IdmsVisitor visitor = new IdmsSubstitutingVisitor(context);
```

Unlike CICS and DB2, IDMS gets no builder seam and no config flag — the fork always substitutes for IDMS today and this preserves that. Adding a seam would cost more upstream lines than it saves. `IdmsDialect.java`'s patch is now +1/−1.

- [ ] **Step 8: Fallback if Step 2 or Step 3 failed**

Revert the grammar reshape (`git checkout HEAD -- server/dialect-idms/src/main/antlr4/org/eclipse/lsp/cobol/dialects/idms/IdmsParser.g4`), keep `IdmsVisitor.java` patched with the minimum needed to compile, and move the ON-path-status decision into the subclass. Restore exactly these two things to `IdmsVisitor.java` (roughly +6/−2 against pristine):

```java
  static ImperativeStatementCallContext imperativeStatementCallOf(IdmsStatementsContext ctx) {
    if (ctx.idmsOptTermStatement() != null) {
      return ctx.idmsOptTermStatement().imperativeStatementCall();
    }
    if (ctx.idmsMandTermStatement() != null) {
      return ctx.idmsMandTermStatement().imperativeStatementCall();
    }
    return null;
  }
```

and change pristine `visitIdmsStatements`'s two `ctx.imperativeStatementCall()` references to `imperativeStatementCallOf(ctx)`. The helper is package-private so `IdmsSubstitutingVisitor` can call it; change the subclass's `ctx.imperativeStatementCall() == null` to `IdmsVisitor.imperativeStatementCallOf(ctx) == null`. The metric then lands at 15 files, ~+307/−65 instead of 14 files, ~+301/−63. Note in the commit message which path was taken and why.

- [ ] **Step 9: Run test to verify it passes**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn -pl dialect-idms -am test
```

Expected: 419 parsing tests green plus `TestPersistentDataExtraction` green including the two new tests.

- [ ] **Step 10: Run all gates end to end**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration/server
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn install -DskipTests
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn test
cd /Users/avisheksengupta/code/cobol-rekt
MAVEN_OPTS="-Djavax.net.ssl.trustStoreType=KeychainStore" mvn test
```

Expected: submodule fully green (`engine` 5177/0, `dialect-idms` 419+/0); parent fully green including `IdmsDialectIntegrationTest` 9/9 and `smojol-core` 108+/0.

`IdmsDialectIntegrationTest` is the load-bearing gate for this task: `dialectContainerNodeCountMatchesIdmsStatementCount` (exactly 4), `allReinjectedNodesCarryIdmsDialect`, `reinjectedNodesReturnNonEmptyOriginalText`, `eachDialectContainerNodeHasDialectNodeFillerParent`, `eachDialectContainerNodeHasExactlyOneChild`, `pureCobolFileHasNoDialectContainerNodes`, and `sequentialParsesResolveNodesCorrectly`.

- [ ] **Step 11: Commit (submodule)**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration
git add -A server/dialect-idms/
git commit -m "refactor: reparent IDMS substitution onto IdmsVisitor

Adds IdmsSubstitutingVisitor and reverts IdmsVisitor.java to pristine 2.5.1.
visitIdmsStatements does not delegate for the ON path-status case: upstream's
addReplacementImperativeStatementContext clears with spaces, leaving no filler
anchor to graft onto.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 9: Update the integration doc, measure, and bump the submodule pointer

**Repository:** parent (and a final measurement in the submodule).

**Files:**
- Modify: `COBOL-LSP-INTEGRATION.md` (§2 patch inventory, §3 re-apply step, §5 constraint)
- Modify: `.talismanrc` if Talisman flags the doc
- Modify: the parent's submodule pointer

**Interfaces:**
- Consumes: the finished state of Tasks 1–8.
- Produces: a `COBOL-LSP-INTEGRATION.md` whose patch inventory matches `git diff --diff-filter=M` output.

- [ ] **Step 1: Measure the final metric**

```bash
cd /Users/avisheksengupta/code/cobol-rekt/che-che4z-lsp-for-cobol-integration
git diff --numstat --diff-filter=M 2.5.1 -- 'server/**/src/main/**' | sort -k3
git diff --numstat --diff-filter=M 2.5.1 -- 'server/**/src/main/**' | awk '{a+=$1;d+=$2;n++} END {print n" files, +"a"/-"d" = "a+d" lines"}'
git diff --numstat --diff-filter=M 2.5.1 -- 'server/**/*.g4' | awk '{a+=$1;d+=$2;n++} END {print n" grammar files, +"a"/-"d" = "a+d" lines"}'
```

Expected: 14 files / ~+301/−63 = ~364 lines (15 / ~372 if Task 8 took the fallback), down from 20 / 514. Grammar: 3 files / ~143 lines, down from 6 / 173. These six files must have dropped off the modified list entirely: `CobolLexer.g4`, `CICSParser.g4`, `Db2SqlParser.g4`, `CICSVisitor.java`, `Db2SqlVisitor.java`, and `IdmsVisitor.java` (the last one only if the reshape succeeded).

Paste the actual output into the Task 9 commit message. If the numbers are materially worse than these targets, find which file kept an unexpected patch before proceeding.

- [ ] **Step 2: Update `COBOL-LSP-INTEGRATION.md`**

Three edits, all driven by Step 1's actual output:

**§2 patch inventory.** Remove the rows for `AnnotatedParserRuleContext`, the `CobolLexer.g4` dialect tokens, `CICSParser.g4`, `Db2SqlParser.g4`, `CICSVisitor.java` and `Db2SqlVisitor.java`. Replace the `CobolParser.g4` dialect row with the surviving three-line delta:

```antlr
conditionalStatementCall : statement | dialectStatement ;
dialectStatement         : dialectIfStatment | dialectNodeFiller ;
dialectNodeFiller        : ZERO_WIDTH_SPACE+ DOT_FS? EOF? ;
```

and note that the `dialectStatement` reorder is load-bearing: upstream leads with a bare `ZERO_WIDTH_SPACE` alternative, which yields no rule context to graft onto.

Add a row for each of the three substituting visitors describing them as thin fork-added subclasses that record a fragment and delegate, and note that `CicsSubstitutingVisitor` was previously a verbatim copy that had silently lost `CICSOptionsCheckUtility`, `cics_abend`/`ExecCicsAbendNode`, `missingEndExec` and `visitVariableNameUsage`.

**§3 re-apply step.** Delete step 4 ("re-apply `dialectNodeFiller` grammar insertions if upstream changed surrounding rules"). Dialect grafting is no longer a reason the grammars are forked; the residue is smojol named-rule extraction, new IDMS statement support, and genuine COBOL fixes.

**§5 constraint.** Restate it: `PersistentData` holds a static fragment list and a static claimed set, it is still not thread-safe by design, and `PersistentData.reset()` **must** be called at `ParsePipeline` entry — which it now is. Without the reset, a sequential parse can claim a positionally identical fragment left over from the previous parse.

- [ ] **Step 3: Verify the doc's claims against the repository**

```bash
cd /Users/avisheksengupta/code/cobol-rekt
grep -n "AnnotatedParserRuleContext\|DIALECT_MARKER\|dialectGuid\|_DIALECT_\|getDialectNode\|setDialectRecursively" COBOL-LSP-INTEGRATION.md
```

Expected: any surviving mention is explicitly historical ("previously", "before 2026-08"). A bare present-tense mention of a deleted symbol is a doc bug — fix it.

- [ ] **Step 4: Bump the submodule pointer**

```bash
cd /Users/avisheksengupta/code/cobol-rekt
git -C che-che4z-lsp-for-cobol-integration rev-parse --abbrev-ref HEAD
git -C che-che4z-lsp-for-cobol-integration status --porcelain
git add che-che4z-lsp-for-cobol-integration
git status --short
```

Expected: the submodule is on `dialect-surface-reduction` with a clean working tree, and `git status --short` shows the pointer staged. Do not merge anything to `poc` or `main` here — that is a separate reviewed step (see Step 6).

- [ ] **Step 5: Commit (parent)**

```bash
cd /Users/avisheksengupta/code/cobol-rekt
git add COBOL-LSP-INTEGRATION.md
git commit -m "docs: record the reduced dialect fork surface

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

If Talisman blocks this commit, add the filename+checksum entry it prints to `.talismanrc` before the trailing `version: "1.0"` line, then re-run the commit. Never use `--no-verify`.

- [ ] **Step 6: Report the merge sequence to the user, do not execute it**

Print, and stop:

```
Ready to merge, in this order:
  1. submodule: merge-2.5.1              -> poc
  2. submodule: dialect-surface-reduction -> poc
  3. parent:    dialect-surface-reduction -> main

Step 1 first, deliberately: dialect-surface-reduction is based on merge-2.5.1,
so merging it into poc brings the 2.5.1 merge along. Landing merge-2.5.1 first
keeps the two merges independently reviewable and revertable.
```

Merging to `poc`/`main` is the user's call, not part of this plan.

---

## Out of Scope

Explicitly not touched by any task above, per spec §3:

- **The static `PersistentData`.** Replacing it with a per-parse instance registry is the one change in this set that would *increase* the metric: smojol grafts at `ParsePipeline.java:160` and nothing in `smojol-core` or `smojol-toolkit` can reach a `DialectProcessingContext`, so the read side would need new patches on `ParserStage`, `ParserStageResult` or `AnalysisContext`. Consequence accepted: parallel parsing stays blocked.
- **The ~65 lines of smojol named-rule extraction in `CobolParser.g4`** — `divisor`, `multiplyLhs`, `charBasedOperator`, `additionalCondition`, `multipleArithmeticExpressions`/`additionalArithmeticExpression`, `nextSentence`/`nextSentenceWrapperStatement`, `mapStatement`, `genericOnClauseStatement`/`onClauseBlock`, and hoisting `NEXT SENTENCE` out of `ifThen`/`ifElse`/`WHEN`. These give smojol named contexts to attach flow nodes to.
- **The ~60 lines of new IDMS statement support** in `IdmsParser.g4`/`IdmsLexer.g4` — `ADD`/`DEL MODULE`/`RECORD`, `LANGUAGE IS COBOL`, `MODULE SOURCE`, and the `transferStatement` mandatory-terminator split. Task 8 reshapes the *containing* rule but must preserve all of these.
- **The genuine COBOL grammar fixes** — `ALTERNATE (RECORD KEY? | KEY)`, `level88DataUsageClause` + `dataValueClause+`, `(GENERIC_COMMENTS NONNUMERICLITERAL)?`, `| ZERO`, `MEMBERS?`, `putReturnClause?`, `checkTerminalMaxLengthClause?`.
- **`AnalysisConfig.java`'s +106/−0**, `CobolVisitor.java`'s +19/−16, `ParserStage.java`'s +12/−13, `DialectService.java`'s +11/−1 and the five 1-3 line patches. `DialectService`'s patch is irreducible: something must choose between the original and substituting visitors.
- **Upstreaming.** The three surviving `CobolParser.g4` lines all widen rules upstream already owns (`ZERO_WIDTH_SPACE`, `DIALECT_IF`, `dialectNodeFiller`, `dialectIfStatment`, `dialectSection`, `dialectStatement` all ship in 2.5.1), so acceptance would take dialect-attributable grammar surface to zero. Filing that PR is follow-on work.
