# COBOL Qualified Name Resolution Fix

**Date:** 2026-05-19  
**Status:** Approved  
**Location:** `smojol-core` module, symbol table subsystem

---

## Problem

smojol silently drops `OF`/`IN` qualifiers when resolving COBOL variable references. `GeneralIdentifierVisitor` extracts only the bare `variableUsageName`, ignoring `inData*` context. `SmojolSymbolTable` is keyed by bare name; `reference(GeneralIdentifierContext)` is a stub returning `null`. The result: ambiguous bare-name matches, incorrect `FLOWS_INTO` edges for programs with duplicate field names across different data structures.

---

## Design

### 1. `QualifiedName` Record

**Package:** `io.smojol.common.ast` (or `io.smojol.common.id`)

```java
public record QualifiedName(List<String> parts) {

    // parts = [innermost, ..., outermost] — COBOL order
    public static QualifiedName of(String bareName) {
        return new QualifiedName(List.of(bareName));
    }

    public static QualifiedName of(String bareName, List<String> qualifiers) {
        // qualifiers = outermost-first as written in source (OF A OF B)
        // reverse to store innermost-first
        var parts = new ArrayList<String>();
        parts.add(bareName);
        var reversed = new ArrayList<>(qualifiers);
        Collections.reverse(reversed);
        parts.addAll(reversed);
        return new QualifiedName(Collections.unmodifiableList(parts));
    }

    public String bareName() { return parts.get(0); }

    /**
     * Returns true if this QualifiedName is a suffix-subsequence of `candidate`.
     * candidate = full path from root to field, outermost-first.
     * this.parts = [bareName, qualifier1, qualifier2, ...] innermost-first.
     *
     * Match succeeds iff parts reversed (outermost-first) form a contiguous
     * subsequence ending at the tail of candidate.
     */
    public boolean isSuffixMatchedBy(List<String> candidate) {
        var query = new ArrayList<>(parts);
        Collections.reverse(query); // outermost-first
        return isSuffixSubsequence(query, candidate);
    }

    private static boolean isSuffixSubsequence(List<String> query, List<String> candidate) {
        if (query.isEmpty()) return true;
        if (candidate.isEmpty()) return false;
        var qHead = query.get(0);
        var qTail = query.subList(1, query.size());
        // Find last occurrence of qHead in candidate, then recurse on tail
        for (int i = candidate.size() - 1; i >= 0; i--) {
            if (candidate.get(i).equalsIgnoreCase(qHead)) {
                return isSuffixSubsequence(qTail, candidate.subList(i + 1, candidate.size()));
            }
        }
        return false;
    }
}
```

**Key property:** `isSuffixMatchedBy` uses subsequence matching (not contiguous substring). This correctly handles COBOL's partial qualification rule where qualifiers need not name every ancestor — they only need to form an unambiguous path from bareName upward.

---

### 2. Pure Recursive Index in `SmojolSymbolTable`

The symbol table builds two indexes from the existing `CobolDataStructure` tree:

- `bareIndex: Map<String, List<CobolDataStructure>>` — bare name → all matching nodes (enables ambiguity detection)
- `qualifiedIndex: Map<QualifiedName, CobolDataStructure>` — unique qualified name → node (for unambiguous lookups)

**Building the index (pure, no mutation):**

```java
// Returns a flat stream of (path, node) pairs via recursive descent
private static Stream<Map.Entry<List<String>, CobolDataStructure>> allPaths(
        CobolDataStructure node, List<String> ancestorPath) {
    var currentPath = Stream.concat(ancestorPath.stream(), Stream.of(node.name())).toList();
    var selfEntry = Map.entry(currentPath, node);
    var childEntries = node.children().stream()
            .flatMap(child -> allPaths(child, currentPath));
    return Stream.concat(Stream.of(selfEntry), childEntries);
}

private static Map<String, List<CobolDataStructure>> buildBareIndex(CobolDataStructure root) {
    return allPaths(root, List.of())
            .collect(Collectors.groupingBy(
                    e -> e.getValue().name(),
                    Collectors.mapping(Map.Entry::getValue, Collectors.toList())
            ));
}
```

The qualified index is derived on demand during `reference()` — see Section 3.

---

### 3. Unified `reference` Method

`SmojolSymbolTable` exposes:

```java
// Public: resolve by bare name (wraps to QualifiedName.of(name))
public CobolDataStructure reference(String bareName) {
    return reference(QualifiedName.of(bareName));
}

// Public: resolve from parse tree context (extracts qualifiers, delegates below)
public CobolDataStructure reference(GeneralIdentifierContext ctx) {
    return reference(extractQualifiedName(ctx));
}

// Private: unified resolution — covers bare and qualified lookups
private CobolDataStructure reference(QualifiedName name) {
    var candidates = bareIndex.getOrDefault(name.bareName(), List.of());
    if (candidates.isEmpty()) return NullDataStructure.INSTANCE;
    if (candidates.size() == 1) return candidates.get(0);

    // Multiple candidates — filter by qualifier match
    var matched = candidates.stream()
            .filter(c -> name.isSuffixMatchedBy(pathOf(c)))
            .toList();
    if (matched.size() == 1) return matched.get(0);
    if (matched.isEmpty()) return NullDataStructure.INSTANCE;
    throw new AmbiguousQualifierException(name, matched);
}

// Extracts QualifiedName from parse tree: qualifiers are inData* nodes, innermost-first
private static QualifiedName extractQualifiedName(GeneralIdentifierContext ctx) {
    var bareName = ctx.qualifiedDataName().variableUsageName().getText();
    var qualifiers = ctx.qualifiedDataName().inData().stream()
            .map(inData -> inData.variableUsageName().getText())
            .toList(); // innermost-first as they appear in source
    return QualifiedName.of(bareName, qualifiers);
}
```

`pathOf(CobolDataStructure node)` returns the full ancestor path (outermost-first) for a node. This can be pre-computed during index build or computed on demand via parent-pointer traversal.

---

### 4. Test Cases

#### `QualifiedName.isSuffixMatchedBy` unit tests

| QualifiedName | Candidate path | Expected |
|---|---|---|
| `FIELD-A` (bare) | `[ROOT, STRUCT-1, FIELD-A]` | `true` |
| `FIELD-A OF STRUCT-1` | `[ROOT, STRUCT-1, FIELD-A]` | `true` |
| `FIELD-A OF STRUCT-2` | `[ROOT, STRUCT-1, FIELD-A]` | `false` |
| `FIELD-A OF ROOT` | `[ROOT, STRUCT-1, FIELD-A]` | `true` (subsequence, ROOT appears before FIELD-A) |
| `FIELD-A OF STRUCT-1 OF ROOT` | `[ROOT, STRUCT-1, FIELD-A]` | `true` |
| `FIELD-A OF MISSING` | `[ROOT, STRUCT-1, FIELD-A]` | `false` |
| `FIELD-A` (bare) | `[]` | `false` |

#### `SmojolSymbolTable` integration tests

Given a data structure:
```
01 ROOT.
   05 STRUCT-1.
      10 FIELD-A  PIC X.
   05 STRUCT-2.
      10 FIELD-A  PIC X.
      10 FIELD-B  PIC X.
```

| Call | Expected |
|---|---|
| `reference("FIELD-B")` | unique `FIELD-B` under `STRUCT-2` |
| `reference("FIELD-A")` | `AmbiguousQualifierException` |
| `reference(QN("FIELD-A", ["STRUCT-1"]))` | `FIELD-A` under `STRUCT-1` |
| `reference(QN("FIELD-A", ["STRUCT-2"]))` | `FIELD-A` under `STRUCT-2` |
| `reference(QN("FIELD-A", ["MISSING"]))` | `NullDataStructure.INSTANCE` |
| `reference("NONEXISTENT")` | `NullDataStructure.INSTANCE` |

---

### 5. Wiring Up Call Sites

After all tests pass, update three call sites:

**`GeneralIdentifierVisitor.visitGeneralIdentifier`**
```java
// Before:
String name = ctx.qualifiedDataName().variableUsageName().getText();
CobolDataStructure resolved = symbolTable.reference(name);

// After:
CobolDataStructure resolved = symbolTable.reference(ctx);
```

**`CobolExpressionBuilder.identifier`**  
Same pattern — replace bare-name extraction with `symbolTable.reference(ctx)`.

**`CobolReferenceBuilder.resolve`**  
Same pattern — replace bare-name extraction with `symbolTable.reference(ctx)`.

`reference(String)` is preserved for programmatic callers that don't have parse tree context (e.g., tests, tooling).

---

## Constraints

- **No mutation:** `buildIndex` uses streams and collectors; `isSuffixMatchedBy` is recursive with immutable sublists
- **No null returns:** unresolved references return `NullDataStructure.INSTANCE`
- **No static methods on domain types:** `extractQualifiedName` is private instance method on `SmojolSymbolTable`
- **TDD order:** write failing tests for `QualifiedName` first, then `SmojolSymbolTable`, then wire call sites
- **No mocks:** tests use real `CobolDataStructure` trees constructed directly

---

## Out of Scope

- Transitive `FLOWS_INTO` traversal (done by the NetworkX layer)
- EXEC CICS opaque blob parsing (separate concern)
- BMS field ↔ COBOL variable name matching (separate concern, bms-tools `graph/` sub-package)
