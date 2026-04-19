# IDMS Dialect Reinjection — Correctness Analysis

_Date: 2026-04-17_

## Background

The smojol fork of che-che4z-lsp-for-cobol implements a two-phase "grafting" mechanism
that allows IDMS DML statements to survive COBOL preprocessing and be reattached to the
final COBOL parse tree.

**Extraction (che4z side)**
`IdmsVisitor.replaceWithMetadata()` is called for each IDMS DML statement. It:
- Calls `PersistentData.next()` to get an incrementing ID ("1", "2", …)
- Stores the IDMS parse tree node in `ctx.customData` under the key `"IDMS-N"`
- Replaces the IDMS statement text with a `_DIALECT_ N .` placeholder in the preprocessed
  COBOL text (all non-whitespace characters are replaced with U+200B zero-width spaces)
- Registers the IDMS parse tree root in `PersistentData.trees` via `addDialectTree()`

**Reinjection (smojol side)**
After the COBOL parser processes the preprocessed text,
`DialectIntegratorListener.enterDialectNodeFiller()` is invoked for each `dialectNodeFiller`
parse tree node (the grammar rule that matched the placeholder). It:
- Extracts the ID N from the `dialectGuid()` child
- Looks up the IDMS node in `PersistentData` by key `"IDMS-N"`
- Attaches a `DialectContainerNode` wrapper as a direct child of the
  `DialectNodeFillerContext`

---

## Key Files

| File | Role |
|------|------|
| `che-che4z-lsp-for-cobol-integration/server/dialect-idms/src/main/java/…/idms/IdmsVisitor.java` | Extraction — `replaceWithMetadata()` |
| `smojol-core/…/idms/DialectIntegratorListener.java` | Reinjection — `enterDialectNodeFiller()` |
| `smojol-core/…/idms/DialectContainerNode.java` | Wrapper node injected at placeholder positions |
| `che-che4z-lsp-for-cobol-integration/server/common/…/poc/PersistentData.java` | Global state store — counter, trees list, DFS lookup |
| `smojol-toolkit/…/pipeline/ParsePipeline.java` | Orchestration — wires extraction → COBOL parse → reinjection |

---

## Correctness Issues

### Issue 1 — ~~HIGH~~ FIXED: Unguarded NPE on missing IDMS key

**Location:** `DialectIntegratorListener.java`

**Fix applied (2026-04-17):** A null guard was added before both dereferences. If `PersistentData.getDialectNode()` returns `null` for a given key, the listener logs a `WARNING` and returns early rather than throwing an undiagnosable `NullPointerException`:

```java
ParseTree dialectNode = PersistentData.getDialectNode("IDMS-" + guid);
if (dialectNode == null) {
    LOGGER.warning("No IDMS node found for key IDMS-" + guid + "; skipping reinjection");
    return;
}
LocalisedDialect dialect = PersistentData.dialect("IDMS-" + guid);
LOGGER.finer(String.format("Restoring _DIALECT_ %s: %s", guid, dialectNode.getText()));
ctx.addChild(new DialectContainerNode(dialectNode, ctx, dialect));
restores++;
```

**Verified by tests (2026-04-17):**

| Test | Assertion | Result |
|------|-----------|--------|
| `missingKeyDoesNotThrowNullPointerException` | no exception when key absent | PASS |
| `missingKeyProducesZeroRestores` | `restores == 0` when key absent | PASS |
| `nullDialectGuidIsHandledGracefully` | pre-existing null-guid guard unaffected | PASS |

Tests live in `smojol-core/…/idms/DialectIntegratorListenerMissingKeyTest`.

---

### Issue 2 — ~~MEDIUM-HIGH~~ CLOSED: Double-extraction of IDMS `IF` statements is a false alarm

**Location:** `IdmsVisitor.java` — `visitIdmsIfStatement` and `visitIdmsIfCondition`

**Original concern:** `visitIdmsIfStatement` calls `replaceWithMetadata` then `visitChildren`,
and `visitIdmsIfCondition` also calls `replaceWithMetadata` — if `IdmsIfConditionContext` were
a child of `IdmsIfStatementContext`, this would produce two extractions per IF statement.

**Grammar analysis (verified 2026-04-17):** `ifStatement` and `idmsIfStatement` are
**siblings** in `idmsRules`, not parent-child:

```antlr
idmsRules
    : ifStatement       // → IF idmsIfCondition
    | idmsIfStatement   // → inquireMapIfStatement (INQUIRE MAP … IF inqMapIfPhrase)
    | …
    ;
```

`idmsIfStatement` expands to `inquireMapIfStatement`, whose IF-phrase child is `inqMapIfPhrase`
— **not** `idmsIfCondition`. Therefore `visitIdmsIfStatement` calling `visitChildren` will
never trigger `visitIdmsIfCondition`. Each grammar path produces exactly one extraction.

**Verified by tests (2026-04-17):** All 5 tests in `TestPersistentDataExtraction` (IF
statement extraction section) pass:

| Test | Expected counter | Result |
|------|-----------------|--------|
| `IF IX-EMP EMPTY MOVE 'X' TO MT-FLAG.` | 1 | PASS |
| `IF NOT IX-EMP MEMBER MOVE 'X' TO MT-FLAG.` | 1 | PASS |
| `INQUIRE MAP EMPMAP IF INPUT CHANGED.` | 1 | PASS |
| Both together | 2 | PASS |
| `getDialectNode("IDMS-1")` after IF extraction | non-null | PASS |

Preprocessed output confirms single extraction per statement:
- `IF IX-EMP EMPTY …` → `IF _DIALECT_ 1 . <zero-width-spaces> MOVE 'X' TO MT-FLAG.`
- `INQUIRE MAP EMPMAP IF INPUT CHANGED.` → `_IF_  _DIALECT_ 1 . <zero-width-spaces>.`

**No fix required.** The design is correct as implemented.

---

### Issue 3 — MEDIUM (REOPENED): No `PersistentData.reset()` between files in production

**Location:** `ParsePipeline.java` — the reinjection block

**History:** A `PersistentData.reset()` call was added after `walker.walk()` on 2026-04-17,
then reverted on 2026-04-18 after discovering that `PersistentData` has a three-phase
lifetime — not just parsing.

**Why reset is wrong here:** `NodeText.dialectOriginalText()` reads from `PersistentData`
*after* `parse()` returns, during program analysis by `IdmsTransferFlowNode` and
`DialectStatementFlowNode`. Resetting `PersistentData` at the end of `parse()` silently
breaks IDMS text recovery for any downstream consumer that calls these flow nodes:

```java
// NodeText.java — called post-parse, during analysis
ParseTree idmsTextNode = PersistentData.getDialectNode("IDMS-" + guid);
return NodeText.originalText(idmsTextNode, NodeText::PASSTHROUGH);
```

If `reset()` runs before this, `getDialectNode()` returns `null` and `originalText()` is
called on `null` — silent null return or NPE depending on the caller.

**Current state:** `PersistentData` accumulates across files in a batch run:

- File 1 extracts IDs 1–4; file 2 extracts IDs 5–8 (counter never resets)
- `trees` grows by one entry per file per implicit dialect (IDMS, CICS, DB2 each call
  `addDialectTree`); in a large batch the list is never trimmed — **memory leak**

**Correct fix (TODO):** Replace the global `PersistentData` with a scoped instance injected
per pipeline run and threaded through to both the che4z extraction side and the smojol
reinjection and analysis sides. This requires changes to the che4z integration.

---

### Issue 4 — MEDIUM: DFS key-collision risk in `PersistentData.getDialectNode()`

**Location:** `PersistentData.java:34–54`

```java
public static ParseTree getDialectNode(String displayOperand) {
    for (AnnotatedParserRuleContext tree : trees) {
        ParseTree dialectNode = getDialectNode(displayOperand, tree);
        if (dialectNode == null) continue;
        return dialectNode;  // returns first match across all registered trees
    }
    return null;
}
```

Key uniqueness is guaranteed only because the global counter accumulates and never resets,
so no two trees ever have the same `"IDMS-N"` key.  This is not enforced by the data
structure — it is an accidental property.

If the counter were ever reset while `trees` still holds old entries (e.g., a partial reset,
a future refactor, or a test that resets the counter but not the trees), the DFS would
silently return the _first_ matching node across all trees, which could be a stale node from
a previously parsed file.  No error is raised; the wrong IDMS subtree would be silently
grafted into the current file's parse tree.

**Fix:** Enforce key uniqueness explicitly, or (better) replace the global list with a map
keyed by run ID so that each pipeline run only sees its own trees.

---

### Issue 5 — LOW-MEDIUM: Synthetic tokens in `DialectContainerNode` have no source positions

**Location:** `DialectContainerNode.java:44–51`

```java
@Override
public Token getStart() {
    return new CommonToken(CobolLexer.COMPUTATIONAL, getText());
}

@Override
public Token getStop() {
    return new CommonToken(CobolLexer.COMPUTATIONAL, getText());
}
```

Problems:

1. **Token type is wrong.** `CobolLexer.COMPUTATIONAL` is the COBOL reserved word for `COMP`.
   It has no semantic relationship to a dialect container node.

2. **No source position.** `new CommonToken(int type, String text)` leaves `startIndex` and
   `stopIndex` at -1.  Any downstream code that calls `token.getStartIndex()` for error
   ranges, code lenses, hover tooltips, or position mapping will receive -1.

3. **O(n) per call.** `getText()` traverses the entire IDMS subtree via
   `NodeText.originalText(dialectNode, NodeText::PASSTHROUGH)` on every invocation of
   `getStart()` and `getStop()`.  If these are called in a hot path (e.g., a walker visiting
   every node), this is quadratic in the size of the IDMS subtree.

**Fix:** Cache `getText()` once in the constructor.  For token type, use
`Token.INVALID_TYPE` or a dedicated synthetic type constant rather than `COMPUTATIONAL`.
Source positions cannot be accurately recovered from the preprocessed text without additional
mapping infrastructure.

---

### Issue 6 — LOW: `DialectContainerNode` child-count invariant breakable from outside

**Location:** `DialectContainerNode.java:21–36`

The constructor adds `dialectNode` via `addAnyChild()` (inherited from
`ParserRuleContext`), but `getChildCount()` is hardcoded to 1 and `getChild(int i)` ignores
`i` and always returns `dialectNode`:

```java
@Override public int getChildCount() { return 1; }
@Override public ParseTree getChild(int i) { return dialectNode; }
```

Because `addChild()` and `addAnyChild()` are publicly inherited from `ParserRuleContext`,
any external caller can silently add children to the internal list.  Those children will
be invisible to `getChild()` and will not be counted by `getChildCount()` — the node
becomes internally inconsistent.  Out-of-bounds calls like `getChild(5)` return
`dialectNode` instead of throwing, masking bugs in callers.

**Fix:** Override `addChild()`/`addAnyChild()` to throw `UnsupportedOperationException`, or
seal the class so subclasses cannot bypass the invariant.

---

## Test Coverage Gaps

The following scenarios are **not covered** by the existing test suite and should be added:

| Scenario | Why it matters |
|----------|----------------|
| ~~IDMS `IF` statement with condition~~ | ~~Exercises the double-extraction path (Issue 2)~~ — **covered** by `TestPersistentDataExtraction` IF section |
| ~~`PersistentData.getDialectNode()` called with unknown key~~ | **covered** by `DialectIntegratorListenerMissingKeyTest` (null guard returns early, logs warning) |
| ~~`DialectIntegratorListener` with missing key in PersistentData~~ | **covered** by `DialectIntegratorListenerMissingKeyTest` — graceful skip verified |
| Batch of 10+ files (memory / counter accumulation) | Catches unbounded `trees` growth (Issue 3) — requires scoped `PersistentData` TODO |
| `NodeText.dialectOriginalText()` after reset | Verifies post-parse text recovery breaks if reset() is called (Issue 3) |
| `getStart()` / `getStop()` called in a position-sensitive context | Verifies -1 is handled or documented |

---

## Related Tests

| Test class | Module | What it covers |
|------------|--------|----------------|
| `IdmsDialectIntegrationTest` | `smojol-toolkit` | Reinjection end-to-end: counts, dialect annotation, text, tree structure, sequential parses |
| `TestPersistentDataExtraction` | `dialect-idms` | Extraction side: counter, key retrieval, dialect annotation, COBOL/IDMS interleaving, reset behaviour, IF-statement extraction (Issue 2) |
