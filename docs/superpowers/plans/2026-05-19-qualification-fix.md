# COBOL Qualified Name Resolution Fix — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace smojol's bare-name-only variable resolution with COBOL `OF`/`IN` qualifier-aware resolution so that `FLOWS_INTO` edges and runtime data references correctly identify fields when the same name appears under multiple parent structures.

**Architecture:** `QualifiedName` is a pure record holding an innermost-first list of name parts; it carries a recursive subsequence `isSuffixMatchedBy` predicate. `CobolDataStructure` gains a lazy `allPaths` stream and a `reference(QualifiedName)` method. `SmojolSymbolTable` builds a qualified index using a new scoped `QualifiedPathIndexVisitor` and properly implements its stub `reference(GeneralIdentifierContext)`. `CobolReferenceBuilder` is updated to extract qualifiers and call `data.reference(QualifiedName)`.

**Tech Stack:** Java 21, JUnit 5, Maven (`mvn test -pl smojol-core -Dtest=ClassName`), no Mockito (real objects only).

---

## File Structure

| Action | Path | Responsibility |
|--------|------|----------------|
| Create | `smojol-core/src/main/java/org/smojol/common/pseudocode/QualifiedName.java` | Immutable record with suffix-subsequence matching |
| Create | `smojol-core/src/main/java/org/smojol/common/pseudocode/AmbiguousQualifierException.java` | Runtime exception for ambiguous qualifier |
| Create | `smojol-core/src/main/java/org/smojol/common/pseudocode/NullSymbolReference.java` | Null-object for SymbolReference (no null returns) |
| Create | `smojol-core/src/main/java/org/smojol/common/pseudocode/QualifiedPathIndexVisitor.java` | Scoped visitor that builds bare-name → (path, ref) index |
| Modify | `smojol-core/src/main/java/org/smojol/common/vm/structure/CobolDataStructure.java` | Add `allPaths(List<String>)` stream + `reference(QualifiedName)` |
| Modify | `smojol-core/src/main/java/org/smojol/common/pseudocode/SmojolSymbolTable.java` | Store root, build qualified index, implement reference stub |
| Modify | `smojol-core/src/main/java/org/smojol/common/vm/reference/CobolReferenceBuilder.java` | Extract qualifiers, call `data.reference(QualifiedName)` |
| Create | `smojol-core/src/test/java/org/smojol/common/QualifiedNameTest.java` | Unit tests for QualifiedName |
| Create | `smojol-core/src/test/java/org/smojol/common/vm/structure/CobolDataStructureTestFactory.java` | Test helper: builds CobolDataStructure trees from NullDataStructure |
| Create | `smojol-core/src/test/java/org/smojol/common/vm/structure/CobolDataStructureQualificationTest.java` | Tests for allPaths + reference(QualifiedName) |
| Create | `smojol-core/src/test/java/org/smojol/common/SmojolSymbolTableQualificationTest.java` | Tests for SmojolSymbolTable qualified lookup |

---

### Task 1: `QualifiedName` record

**Files:**
- Create: `smojol-core/src/main/java/org/smojol/common/pseudocode/QualifiedName.java`
- Create: `smojol-core/src/test/java/org/smojol/common/QualifiedNameTest.java`

- [ ] **Step 1: Write the failing tests**

```java
// smojol-core/src/test/java/org/smojol/common/QualifiedNameTest.java
package org.smojol.common;

import org.junit.jupiter.api.Test;
import org.smojol.common.pseudocode.QualifiedName;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class QualifiedNameTest {

    // isSuffixMatchedBy — bare name

    @Test
    public void bareNameMatchesAnyPathEndingInThatName() {
        QualifiedName qn = QualifiedName.of("FIELD-A");
        assertTrue(qn.isSuffixMatchedBy(List.of("ROOT", "STRUCT-1", "FIELD-A")));
    }

    @Test
    public void bareNameDoesNotMatchPathNotEndingInThatName() {
        QualifiedName qn = QualifiedName.of("FIELD-A");
        assertFalse(qn.isSuffixMatchedBy(List.of("ROOT", "FIELD-A", "CHILD")));
    }

    @Test
    public void bareNameDoesNotMatchEmptyPath() {
        QualifiedName qn = QualifiedName.of("FIELD-A");
        assertFalse(qn.isSuffixMatchedBy(List.of()));
    }

    // isSuffixMatchedBy — qualified name (contiguous qualifiers)

    @Test
    public void qualifiedNameMatchesWhenQualifiersFormSuffixSubsequence() {
        // FIELD-A OF STRUCT-1: parts=[FIELD-A, STRUCT-1]
        // query reversed = [STRUCT-1, FIELD-A]
        // candidate = [ROOT, STRUCT-1, FIELD-A] — suffix [STRUCT-1, FIELD-A] matches
        QualifiedName qn = QualifiedName.of("FIELD-A", List.of("STRUCT-1"));
        assertTrue(qn.isSuffixMatchedBy(List.of("ROOT", "STRUCT-1", "FIELD-A")));
    }

    @Test
    public void qualifiedNameDoesNotMatchWrongParent() {
        QualifiedName qn = QualifiedName.of("FIELD-A", List.of("STRUCT-2"));
        assertFalse(qn.isSuffixMatchedBy(List.of("ROOT", "STRUCT-1", "FIELD-A")));
    }

    // isSuffixMatchedBy — partial qualification (non-contiguous ancestors)

    @Test
    public void qualifierCanSkipIntermediateAncestors() {
        // FIELD-A OF ROOT: parts=[FIELD-A, ROOT]
        // query reversed = [ROOT, FIELD-A]
        // candidate = [ROOT, STRUCT-1, FIELD-A]
        // ROOT is at index 0, FIELD-A is at index 2 → subsequence match
        QualifiedName qn = QualifiedName.of("FIELD-A", List.of("ROOT"));
        assertTrue(qn.isSuffixMatchedBy(List.of("ROOT", "STRUCT-1", "FIELD-A")));
    }

    @Test
    public void fullQualificationMatchesExactPath() {
        // FIELD-A OF STRUCT-1 OF ROOT: parts=[FIELD-A, STRUCT-1, ROOT]
        QualifiedName qn = QualifiedName.of("FIELD-A", List.of("STRUCT-1", "ROOT"));
        assertTrue(qn.isSuffixMatchedBy(List.of("ROOT", "STRUCT-1", "FIELD-A")));
    }

    @Test
    public void missingQualifierNameFailsMatch() {
        QualifiedName qn = QualifiedName.of("FIELD-A", List.of("MISSING"));
        assertFalse(qn.isSuffixMatchedBy(List.of("ROOT", "STRUCT-1", "FIELD-A")));
    }

    // bareName accessor

    @Test
    public void bareNameReturnsFirstPart() {
        QualifiedName qn = QualifiedName.of("FIELD-A", List.of("STRUCT-1"));
        assertEquals("FIELD-A", qn.bareName());
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
cd ~/code/smojol
mvn test -pl smojol-core -Dtest=QualifiedNameTest 2>&1 | tail -20
```

Expected: compilation error — `QualifiedName` does not exist yet.

- [ ] **Step 3: Implement `QualifiedName`**

```java
// smojol-core/src/main/java/org/smojol/common/pseudocode/QualifiedName.java
package org.smojol.common.pseudocode;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

public record QualifiedName(List<String> parts) {

    // parts = [bareName, qualifier1, qualifier2, ...] innermost-first
    // e.g. "FIELD-A OF STRUCT-1" → parts = ["FIELD-A", "STRUCT-1"]

    public static QualifiedName of(String bareName) {
        return new QualifiedName(List.of(bareName));
    }

    // qualifiers: innermost-first as they appear after the first OF/IN in source
    // e.g. "FIELD-A OF STRUCT-1 OF ROOT" → bareName="FIELD-A", qualifiers=["STRUCT-1", "ROOT"]
    public static QualifiedName of(String bareName, List<String> qualifiers) {
        return new QualifiedName(
            Stream.concat(Stream.of(bareName), qualifiers.stream()).toList()
        );
    }

    public String bareName() {
        return parts.get(0);
    }

    /**
     * Returns true if this QualifiedName is a suffix-subsequence of {@code candidate}.
     *
     * {@code candidate} = full root-to-leaf path, outermost-first, e.g. ["ROOT", "STRUCT-1", "FIELD-A"].
     * {@code parts}     = [bareName, qualifier1, ...] innermost-first.
     *
     * The check: reverse parts to outermost-first order, then verify they form a
     * (non-contiguous) subsequence that ends at the tail of candidate.
     * The last element of the reversed query must match the last element of candidate.
     */
    public boolean isSuffixMatchedBy(List<String> candidate) {
        var query = new ArrayList<>(parts);
        Collections.reverse(query); // now outermost-first: [..., qualifier1, bareName]
        return isSuffixSubsequence(query, candidate);
    }

    private static boolean isSuffixSubsequence(List<String> query, List<String> candidate) {
        if (query.isEmpty()) return true;
        if (candidate.isEmpty()) return false;
        var qHead = query.get(0);
        var qTail = query.subList(1, query.size());
        // Find the leftmost occurrence of qHead in candidate, then recurse on the remainder
        for (int i = 0; i < candidate.size(); i++) {
            if (candidate.get(i).equalsIgnoreCase(qHead)) {
                return isSuffixSubsequence(qTail, candidate.subList(i + 1, candidate.size()));
            }
        }
        return false;
    }
}
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
cd ~/code/smojol
mvn test -pl smojol-core -Dtest=QualifiedNameTest 2>&1 | tail -20
```

Expected: `BUILD SUCCESS`, 9 tests pass.

- [ ] **Step 5: Commit**

```bash
cd ~/code/smojol
git add smojol-core/src/main/java/org/smojol/common/pseudocode/QualifiedName.java \
        smojol-core/src/test/java/org/smojol/common/QualifiedNameTest.java
git commit -m "feat: Add QualifiedName record with suffix-subsequence matching"
```

---

### Task 2: `AmbiguousQualifierException` and `NullSymbolReference`

**Files:**
- Create: `smojol-core/src/main/java/org/smojol/common/pseudocode/AmbiguousQualifierException.java`
- Create: `smojol-core/src/main/java/org/smojol/common/pseudocode/NullSymbolReference.java`

No separate tests — these are structural types verified by compilation.

- [ ] **Step 1: Create `AmbiguousQualifierException`**

```java
// smojol-core/src/main/java/org/smojol/common/pseudocode/AmbiguousQualifierException.java
package org.smojol.common.pseudocode;

import java.util.List;

public class AmbiguousQualifierException extends RuntimeException {
    private final QualifiedName qualifiedName;
    private final List<?> candidates;

    public AmbiguousQualifierException(QualifiedName qualifiedName, List<?> candidates) {
        super("Ambiguous reference to '" + qualifiedName.bareName()
              + "': " + candidates.size() + " candidates match qualifier "
              + qualifiedName.parts());
        this.qualifiedName = qualifiedName;
        this.candidates = candidates;
    }

    public QualifiedName qualifiedName() { return qualifiedName; }
    public List<?> candidates() { return candidates; }
}
```

- [ ] **Step 2: Create `NullSymbolReference`**

```java
// smojol-core/src/main/java/org/smojol/common/pseudocode/NullSymbolReference.java
package org.smojol.common.pseudocode;

public class NullSymbolReference extends SymbolReference {
    public static final NullSymbolReference INSTANCE = new NullSymbolReference();

    private NullSymbolReference() {
        super("__NULL__");
    }
}
```

- [ ] **Step 3: Verify compilation**

```bash
cd ~/code/smojol
mvn compile -pl smojol-core 2>&1 | tail -10
```

Expected: `BUILD SUCCESS`.

- [ ] **Step 4: Commit**

```bash
cd ~/code/smojol
git add smojol-core/src/main/java/org/smojol/common/pseudocode/AmbiguousQualifierException.java \
        smojol-core/src/main/java/org/smojol/common/pseudocode/NullSymbolReference.java
git commit -m "feat: Add AmbiguousQualifierException and NullSymbolReference"
```

---

### Task 3: `CobolDataStructure.allPaths()` and `CobolDataStructure.reference(QualifiedName)`

**Files:**
- Modify: `smojol-core/src/main/java/org/smojol/common/vm/structure/CobolDataStructure.java`
- Create: `smojol-core/src/test/java/org/smojol/common/vm/structure/CobolDataStructureTestFactory.java`
- Create: `smojol-core/src/test/java/org/smojol/common/vm/structure/CobolDataStructureQualificationTest.java`

The test helper lives in `org.smojol.common.vm.structure` (same package as `CobolDataStructure`) so it can access the `protected List<CobolDataStructure> structures` field.

- [ ] **Step 1: Create the test factory**

```java
// smojol-core/src/test/java/org/smojol/common/vm/structure/CobolDataStructureTestFactory.java
package org.smojol.common.vm.structure;

import java.util.Arrays;
import java.util.ArrayList;

/**
 * Builds minimal CobolDataStructure trees for testing.
 * Lives in the same package as CobolDataStructure to access the protected 'structures' field.
 * Uses NullDataStructure as a convenient no-op concrete implementation.
 */
public class CobolDataStructureTestFactory {

    /** Create a leaf node with the given name. */
    public static CobolDataStructure leaf(String name) {
        return new NullDataStructure(name);
    }

    /** Create a node with the given name and attach children to it. */
    public static CobolDataStructure node(String name, CobolDataStructure... children) {
        CobolDataStructure n = new NullDataStructure(name);
        n.structures = new ArrayList<>(Arrays.asList(children));
        return n;
    }
}
```

- [ ] **Step 2: Write the failing tests**

```java
// smojol-core/src/test/java/org/smojol/common/vm/structure/CobolDataStructureQualificationTest.java
package org.smojol.common.vm.structure;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.smojol.common.pseudocode.AmbiguousQualifierException;
import org.smojol.common.pseudocode.QualifiedName;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.smojol.common.vm.structure.CobolDataStructureTestFactory.*;

public class CobolDataStructureQualificationTest {

    /*
     * Test tree:
     *   ROOT
     *   ├── STRUCT-1
     *   │   └── FIELD-A
     *   └── STRUCT-2
     *       ├── FIELD-A   (duplicate bare name)
     *       └── FIELD-B
     */
    private CobolDataStructure root;
    private CobolDataStructure fieldAUnderStruct1;
    private CobolDataStructure fieldAUnderStruct2;
    private CobolDataStructure fieldB;

    @BeforeEach
    public void setUp() {
        fieldAUnderStruct1 = leaf("FIELD-A");
        fieldAUnderStruct2 = leaf("FIELD-A");
        fieldB = leaf("FIELD-B");
        root = node("ROOT",
            node("STRUCT-1", fieldAUnderStruct1),
            node("STRUCT-2", fieldAUnderStruct2, fieldB)
        );
    }

    // allPaths tests

    @Test
    public void allPathsIncludesRootAsFirstEntry() {
        var paths = root.allPaths(List.of()).map(Map.Entry::getKey).toList();
        assertEquals(List.of("ROOT"), paths.get(0));
    }

    @Test
    public void allPathsIncludesAllNodes() {
        var paths = root.allPaths(List.of()).map(Map.Entry::getKey).toList();
        // ROOT, STRUCT-1, FIELD-A(1), STRUCT-2, FIELD-A(2), FIELD-B
        assertEquals(6, paths.size());
    }

    @Test
    public void allPathsRecordsFullAncestorChain() {
        var paths = root.allPaths(List.of()).map(Map.Entry::getKey).toList();
        assertTrue(paths.contains(List.of("ROOT", "STRUCT-1", "FIELD-A")));
        assertTrue(paths.contains(List.of("ROOT", "STRUCT-2", "FIELD-A")));
        assertTrue(paths.contains(List.of("ROOT", "STRUCT-2", "FIELD-B")));
    }

    // reference(QualifiedName) tests

    @Test
    public void referenceByBareNameReturnsUniqueMatch() {
        CobolDataStructure resolved = root.reference(QualifiedName.of("FIELD-B"));
        assertSame(fieldB, resolved);
    }

    @Test
    public void referenceByQualifiedNameDisambiguatesDuplicates() {
        CobolDataStructure resolved = root.reference(
            QualifiedName.of("FIELD-A", List.of("STRUCT-1")));
        assertSame(fieldAUnderStruct1, resolved);
    }

    @Test
    public void referenceByQualifiedNamePicksCorrectDuplicate() {
        CobolDataStructure resolved = root.reference(
            QualifiedName.of("FIELD-A", List.of("STRUCT-2")));
        assertSame(fieldAUnderStruct2, resolved);
    }

    @Test
    public void referenceWithSkippedAncestorQualifierWorks() {
        // FIELD-A OF ROOT skips STRUCT-1 — still valid (subsequence)
        CobolDataStructure resolved = root.reference(
            QualifiedName.of("FIELD-A", List.of("ROOT")));
        // Matches BOTH FIELD-A nodes (ROOT appears in both paths) → ambiguous
        assertThrows(AmbiguousQualifierException.class,
            () -> root.reference(QualifiedName.of("FIELD-A", List.of("ROOT"))));
    }

    @Test
    public void referenceWithUnknownQualifierReturnsNullDataStructure() {
        CobolDataStructure resolved = root.reference(
            QualifiedName.of("FIELD-A", List.of("MISSING")));
        assertInstanceOf(NullDataStructure.class, resolved);
    }

    @Test
    public void referenceOfNonExistentNameReturnsNullDataStructure() {
        CobolDataStructure resolved = root.reference(QualifiedName.of("NONEXISTENT"));
        assertInstanceOf(NullDataStructure.class, resolved);
    }

    @Test
    public void ambiguousBareNameThrowsException() {
        // Both STRUCT-1 and STRUCT-2 have FIELD-A; bare name is ambiguous
        assertThrows(AmbiguousQualifierException.class,
            () -> root.reference(QualifiedName.of("FIELD-A")));
    }
}
```

- [ ] **Step 3: Run tests to verify they fail**

```bash
cd ~/code/smojol
mvn test -pl smojol-core -Dtest=CobolDataStructureQualificationTest 2>&1 | tail -20
```

Expected: compilation error — `allPaths` and `reference(QualifiedName)` do not exist yet.

- [ ] **Step 4: Add `allPaths` and `reference(QualifiedName)` to `CobolDataStructure`**

Open `smojol-core/src/main/java/org/smojol/common/vm/structure/CobolDataStructure.java`.

Add the following imports at the top (after existing imports):
```java
import java.util.stream.Stream;
import org.smojol.common.pseudocode.AmbiguousQualifierException;
import org.smojol.common.pseudocode.QualifiedName;
```

Add the following concrete methods to the class body (after the existing `reference(String)` method):

```java
/**
 * Returns a stream of (fullPath, node) pairs for this node and all descendants.
 * fullPath is a root-to-leaf list of names, outermost-first.
 * Call as: root.allPaths(List.of())
 */
public Stream<Map.Entry<List<String>, CobolDataStructure>> allPaths(List<String> ancestorPath) {
    var currentPath = Stream.concat(ancestorPath.stream(), Stream.of(name())).toList();
    return Stream.concat(
        Stream.of(Map.entry(currentPath, this)),
        structures.stream().flatMap(child -> child.allPaths(currentPath))
    );
}

/**
 * Resolves a variable reference using COBOL OF/IN qualifier semantics.
 * Returns NullDataStructure if no match; throws AmbiguousQualifierException if multiple match.
 */
public CobolDataStructure reference(QualifiedName qualifiedName) {
    var candidates = allPaths(List.of())
        .filter(e -> e.getValue().name().equals(qualifiedName.bareName())
                     && qualifiedName.isSuffixMatchedBy(e.getKey()))
        .map(Map.Entry::getValue)
        .toList();
    if (candidates.isEmpty()) return new NullDataStructure(qualifiedName.bareName());
    if (candidates.size() == 1) return candidates.get(0);
    throw new AmbiguousQualifierException(qualifiedName, candidates);
}
```

Also add the import for `Map.Entry` if not already present:
```java
import java.util.Map;
```

- [ ] **Step 5: Run tests to verify they pass**

```bash
cd ~/code/smojol
mvn test -pl smojol-core -Dtest=CobolDataStructureQualificationTest 2>&1 | tail -20
```

Expected: `BUILD SUCCESS`, all tests pass.

- [ ] **Step 6: Commit**

```bash
cd ~/code/smojol
git add smojol-core/src/main/java/org/smojol/common/vm/structure/CobolDataStructure.java \
        smojol-core/src/test/java/org/smojol/common/vm/structure/CobolDataStructureTestFactory.java \
        smojol-core/src/test/java/org/smojol/common/vm/structure/CobolDataStructureQualificationTest.java
git commit -m "feat: Add allPaths stream and reference(QualifiedName) to CobolDataStructure"
```

---

### Task 4: `SmojolSymbolTable` — qualified index and `reference(GeneralIdentifierContext)`

**Files:**
- Create: `smojol-core/src/main/java/org/smojol/common/pseudocode/QualifiedPathIndexVisitor.java`
- Modify: `smojol-core/src/main/java/org/smojol/common/pseudocode/SmojolSymbolTable.java`
- Create: `smojol-core/src/test/java/org/smojol/common/SmojolSymbolTableQualificationTest.java`

The test uses `CobolDataStructureTestFactory` (from Task 3) and builds `SmojolSymbolTable` directly with a test tree. `GeneralIdentifierContext` is constructed by parsing a minimal COBOL snippet.

- [ ] **Step 1: Write the failing test**

```java
// smojol-core/src/test/java/org/smojol/common/SmojolSymbolTableQualificationTest.java
package org.smojol.common;

import com.mojo.algorithms.id.IncrementalIdProvider;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.eclipse.lsp.cobol.core.CobolLexer;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.smojol.common.pseudocode.*;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.NullDataStructure;

import static org.junit.jupiter.api.Assertions.*;
import static org.smojol.common.vm.structure.CobolDataStructureTestFactory.*;

public class SmojolSymbolTableQualificationTest {

    /*
     * Same tree as CobolDataStructureQualificationTest:
     *   ROOT
     *   ├── STRUCT-1
     *   │   └── FIELD-A
     *   └── STRUCT-2
     *       ├── FIELD-A   (duplicate)
     *       └── FIELD-B
     */
    private SmojolSymbolTable symbolTable;
    private CobolDataStructure fieldAUnderStruct1;
    private CobolDataStructure fieldAUnderStruct2;
    private CobolDataStructure fieldB;

    @BeforeEach
    public void setUp() {
        fieldAUnderStruct1 = leaf("FIELD-A");
        fieldAUnderStruct2 = leaf("FIELD-A");
        fieldB = leaf("FIELD-B");
        CobolDataStructure root = node("ROOT",
            node("STRUCT-1", fieldAUnderStruct1),
            node("STRUCT-2", fieldAUnderStruct2, fieldB)
        );
        symbolTable = new SmojolSymbolTable(root, new SymbolReferenceBuilder(new IncrementalIdProvider()));
    }

    @Test
    public void referenceByStringReturnsSymbolReferenceForUniqueName() {
        SymbolReference ref = symbolTable.reference("FIELD-B");
        assertNotNull(ref);
        assertEquals("FIELD-B", ref.id());
    }

    @Test
    public void referenceByGeneralIdentifierWithQualifierDisambiguates() {
        // Parse "FIELD-A OF STRUCT-1" as a GeneralIdentifier
        CobolParser.GeneralIdentifierContext ctx = parseGeneralIdentifier("FIELD-A OF STRUCT-1");
        SymbolReference ref = symbolTable.reference(ctx);
        assertNotNull(ref);
        assertNotInstanceOf(NullSymbolReference.class, ref);
        assertEquals("FIELD-A", ref.id());
    }

    @Test
    public void referenceByGeneralIdentifierForNonexistentNameReturnsNull() {
        CobolParser.GeneralIdentifierContext ctx = parseGeneralIdentifier("NONEXISTENT");
        SymbolReference ref = symbolTable.reference(ctx);
        assertInstanceOf(NullSymbolReference.class, ref);
    }

    @Test
    public void referenceByGeneralIdentifierWithWrongQualifierReturnsNull() {
        CobolParser.GeneralIdentifierContext ctx = parseGeneralIdentifier("FIELD-A OF MISSING");
        SymbolReference ref = symbolTable.reference(ctx);
        assertInstanceOf(NullSymbolReference.class, ref);
    }

    /** Parse a variable reference expression using the COBOL ANTLR grammar. */
    private static CobolParser.GeneralIdentifierContext parseGeneralIdentifier(String text) {
        // Wrap in minimal COBOL context so the parser can resolve generalIdentifier
        // Use the qualifiedDataName rule directly via a test-only entry point
        CobolLexer lexer = new CobolLexer(CharStreams.fromString(text));
        CobolParser parser = new CobolParser(new CommonTokenStream(lexer));
        return parser.generalIdentifier();
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
cd ~/code/smojol
mvn test -pl smojol-core -Dtest=SmojolSymbolTableQualificationTest 2>&1 | tail -20
```

Expected: compilation or assertion failure — `reference(GeneralIdentifierContext)` returns null.

- [ ] **Step 3: Create `QualifiedPathIndexVisitor`**

```java
// smojol-core/src/main/java/org/smojol/common/pseudocode/QualifiedPathIndexVisitor.java
package org.smojol.common.pseudocode;

import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ScopedDataStructureVisitor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

/**
 * Scoped visitor that builds a bare-name index:
 *   bareName → list of (fullPath, SymbolReference) pairs
 *
 * Returned visitor for each child carries the updated path, so the full
 * ancestor chain is tracked without any mutation of shared state.
 * The 'index' map is shared (accumulated across all nodes during traversal).
 */
class QualifiedPathIndexVisitor implements ScopedDataStructureVisitor {

    private final Map<String, List<Map.Entry<List<String>, SymbolReference>>> index;
    private final SymbolReferenceBuilder builder;
    private final List<String> currentPath;

    QualifiedPathIndexVisitor(
            Map<String, List<Map.Entry<List<String>, SymbolReference>>> index,
            SymbolReferenceBuilder builder,
            List<String> currentPath) {
        this.index = index;
        this.builder = builder;
        this.currentPath = currentPath;
    }

    @Override
    public ScopedDataStructureVisitor visit(CobolDataStructure data) {
        var childPath = Stream.concat(currentPath.stream(), Stream.of(data.name())).toList();
        var ref = builder.recordReference(data);
        index.computeIfAbsent(data.name(), k -> new ArrayList<>()).add(Map.entry(childPath, ref));
        return new QualifiedPathIndexVisitor(index, builder, childPath);
    }
}
```

- [ ] **Step 4: Update `SmojolSymbolTable`**

Replace the full content of `SmojolSymbolTable.java` with:

```java
// smojol-core/src/main/java/org/smojol/common/pseudocode/SmojolSymbolTable.java
package org.smojol.common.pseudocode;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ScopedDataStructureVisitor;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SmojolSymbolTable {

    // Legacy bare-name map — preserved for callers using reference(String)
    Map<String, SymbolReference> symbols = new HashMap<>();

    // Qualified index: bareName → [(fullPath, SymbolReference)]
    private final Map<String, List<Map.Entry<List<String>, SymbolReference>>> qualifiedIndex
            = new HashMap<>();

    public SmojolSymbolTable(
            CobolDataStructure dataStructures, SymbolReferenceBuilder symbolReferenceBuilder) {
        // Build legacy bare-name map (unchanged behaviour)
        ScopedDataStructureVisitor legacyVisitor =
                new SymbolTableVisitor(symbols, symbolReferenceBuilder);
        dataStructures.acceptScopedVisitor(legacyVisitor);

        // Build qualified index
        ScopedDataStructureVisitor qualifiedVisitor =
                new QualifiedPathIndexVisitor(qualifiedIndex, symbolReferenceBuilder, List.of());
        dataStructures.acceptScopedVisitor(qualifiedVisitor);
    }

    public SymbolReference reference(String symbolName) {
        return symbols.get(symbolName);
    }

    public void add(SymbolReference reference) {
        symbols.put(reference.id(), reference);
    }

    public SymbolReference reference(CobolParser.GeneralIdentifierContext ctx) {
        QualifiedName qualifiedName = extractQualifiedName(ctx);
        return resolveQualified(qualifiedName);
    }

    // ---- private helpers ----

    private SymbolReference resolveQualified(QualifiedName qualifiedName) {
        var candidates = qualifiedIndex.getOrDefault(qualifiedName.bareName(), List.of())
                .stream()
                .filter(e -> qualifiedName.isSuffixMatchedBy(e.getKey()))
                .map(Map.Entry::getValue)
                .toList();
        if (candidates.isEmpty()) return NullSymbolReference.INSTANCE;
        if (candidates.size() == 1) return candidates.get(0);
        throw new AmbiguousQualifierException(qualifiedName, candidates);
    }

    private static QualifiedName extractQualifiedName(
            CobolParser.GeneralIdentifierContext ctx) {
        var qualifiedDataName = ctx.qualifiedDataName();
        if (qualifiedDataName == null) {
            // functionCall or specialRegister — no variable name
            return QualifiedName.of("__NONVARIABLE__");
        }
        var bareName = qualifiedDataName.variableUsageName().getText();
        // inData() returns qualifiers in source order: innermost-first after the OF/IN keyword
        var qualifiers = qualifiedDataName.inData().stream()
                .map(inData -> inData.variableUsageName().getText())
                .toList();
        return QualifiedName.of(bareName, qualifiers);
    }
}
```

- [ ] **Step 5: Run tests to verify they pass**

```bash
cd ~/code/smojol
mvn test -pl smojol-core -Dtest=SmojolSymbolTableQualificationTest 2>&1 | tail -20
```

Expected: `BUILD SUCCESS`, all tests pass.

> **Note:** If `CobolLexer`/`CobolParser` are not in `smojol-core`'s test classpath, check the module's `pom.xml` test dependencies. The ANTLR grammar jars are likely in `smojol-core`'s compile scope already (used by `GeneralIdentifierVisitor`).

- [ ] **Step 6: Run full smojol-core test suite to check for regressions**

```bash
cd ~/code/smojol
mvn test -pl smojol-core 2>&1 | tail -30
```

Expected: `BUILD SUCCESS`.

- [ ] **Step 7: Commit**

```bash
cd ~/code/smojol
git add smojol-core/src/main/java/org/smojol/common/pseudocode/QualifiedPathIndexVisitor.java \
        smojol-core/src/main/java/org/smojol/common/pseudocode/SmojolSymbolTable.java \
        smojol-core/src/test/java/org/smojol/common/SmojolSymbolTableQualificationTest.java
git commit -m "feat: Implement qualified name resolution in SmojolSymbolTable"
```

---

### Task 5: Fix `CobolReferenceBuilder` to use qualified names

**Files:**
- Modify: `smojol-core/src/main/java/org/smojol/common/vm/reference/CobolReferenceBuilder.java`

The private `resolve(QualifiedDataNameContext, CobolDataStructure)` currently does:
```java
CobolDataStructure reference = data.reference(qualifiedDataNameContext.variableUsageName().getText());
```
Replace the bare-name call with a qualified one.

- [ ] **Step 1: Write a failing test for the fix**

Add a new test class:

```java
// smojol-core/src/test/java/org/smojol/common/vm/reference/CobolReferenceBuilderQualificationTest.java
package org.smojol.common.vm.reference;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.eclipse.lsp.cobol.core.CobolLexer;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.junit.jupiter.api.Test;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.NullDataStructure;

import static org.junit.jupiter.api.Assertions.*;
import static org.smojol.common.vm.structure.CobolDataStructureTestFactory.*;

public class CobolReferenceBuilderQualificationTest {

    /*
     * Tree:  ROOT → STRUCT-1 → FIELD-A
     *             → STRUCT-2 → FIELD-A (duplicate)
     *                        → FIELD-B
     */
    @Test
    public void resolvePicksCorrectNodeWhenQualifierPresent() {
        CobolDataStructure fieldAUnderStruct1 = leaf("FIELD-A");
        CobolDataStructure fieldAUnderStruct2 = leaf("FIELD-A");
        CobolDataStructure root = node("ROOT",
            node("STRUCT-1", fieldAUnderStruct1),
            node("STRUCT-2", fieldAUnderStruct2, leaf("FIELD-B"))
        );

        CobolParser.GeneralIdentifierContext ctx = parseGeneralIdentifier("FIELD-A OF STRUCT-1");

        CobolReferenceBuilder builder = new CobolReferenceBuilder();
        CobolDataStructure resolved = builder.resolve(ctx, root);

        assertSame(fieldAUnderStruct1, resolved);
    }

    @Test
    public void resolveByBareNameWorksForUniqueField() {
        CobolDataStructure fieldB = leaf("FIELD-B");
        CobolDataStructure root = node("ROOT",
            node("STRUCT-1", leaf("FIELD-A")),
            node("STRUCT-2", leaf("FIELD-A"), fieldB)
        );

        CobolParser.GeneralIdentifierContext ctx = parseGeneralIdentifier("FIELD-B");

        CobolReferenceBuilder builder = new CobolReferenceBuilder();
        CobolDataStructure resolved = builder.resolve(ctx, root);

        assertSame(fieldB, resolved);
    }

    private static CobolParser.GeneralIdentifierContext parseGeneralIdentifier(String text) {
        CobolLexer lexer = new CobolLexer(CharStreams.fromString(text));
        CobolParser parser = new CobolParser(new CommonTokenStream(lexer));
        return parser.generalIdentifier();
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

```bash
cd ~/code/smojol
mvn test -pl smojol-core -Dtest=CobolReferenceBuilderQualificationTest 2>&1 | tail -20
```

Expected: the `resolvePicksCorrectNodeWhenQualifierPresent` test fails because the current code drops qualifiers.

- [ ] **Step 3: Update `CobolReferenceBuilder.resolve`**

Open `smojol-core/src/main/java/org/smojol/common/vm/reference/CobolReferenceBuilder.java`.

Add imports:
```java
import org.smojol.common.pseudocode.QualifiedName;
import java.util.stream.Collectors;
```

Replace the private static `resolve(QualifiedDataNameContext, CobolDataStructure)` method. The current body is:
```java
private static CobolDataStructure resolve(
    CobolParser.QualifiedDataNameContext qualifiedDataNameContext, CobolDataStructure data) {
  CobolDataStructure reference =
      data.reference(qualifiedDataNameContext.variableUsageName().getText());
  if (qualifiedDataNameContext.tableCall() == null) return reference;
  // ... table call handling unchanged ...
}
```

Change only the first two lines of the method (the `reference` lookup). Replace:
```java
  CobolDataStructure reference =
      data.reference(qualifiedDataNameContext.variableUsageName().getText());
```
With:
```java
  var bareName = qualifiedDataNameContext.variableUsageName().getText();
  var qualifiers = qualifiedDataNameContext.inData().stream()
          .map(inData -> inData.variableUsageName().getText())
          .toList();
  CobolDataStructure reference = data.reference(QualifiedName.of(bareName, qualifiers));
```

Leave the rest of the method (table call handling) unchanged.

- [ ] **Step 4: Run test to verify it passes**

```bash
cd ~/code/smojol
mvn test -pl smojol-core -Dtest=CobolReferenceBuilderQualificationTest 2>&1 | tail -20
```

Expected: `BUILD SUCCESS`, both tests pass.

- [ ] **Step 5: Run full smojol-core test suite to check for regressions**

```bash
cd ~/code/smojol
mvn test -pl smojol-core 2>&1 | tail -30
```

Expected: `BUILD SUCCESS`.

- [ ] **Step 6: Run broader test suite**

```bash
cd ~/code/smojol
mvn test -pl smojol-core,smojol-toolkit 2>&1 | tail -40
```

Expected: `BUILD SUCCESS`. If tests fail, check whether the `AmbiguousQualifierException` is thrown for programs with duplicate field names that previously used bare-name resolution — those programs need `OF`/`IN` qualifiers in the source to be well-formed.

- [ ] **Step 7: Commit**

```bash
cd ~/code/smojol
git add smojol-core/src/main/java/org/smojol/common/vm/reference/CobolReferenceBuilder.java \
        smojol-core/src/test/java/org/smojol/common/vm/reference/CobolReferenceBuilderQualificationTest.java
git commit -m "feat: Fix CobolReferenceBuilder to respect OF/IN qualifiers"
```

---

## Self-Review

### Spec coverage

| Spec section | Covered by |
|---|---|
| `QualifiedName` record with `isSuffixMatchedBy` | Task 1 |
| `AmbiguousQualifierException` | Task 2 |
| Pure recursive/stream `buildBareIndex` | Task 4 (`QualifiedPathIndexVisitor`, `CobolDataStructure.allPaths`) |
| Unified `reference(QualifiedName)` private method | Task 3 (`CobolDataStructure.reference(QualifiedName)`) + Task 4 (`SmojolSymbolTable.resolveQualified`) |
| `reference(String)` wraps to `QualifiedName` | Not required — existing `reference(String)` on `SmojolSymbolTable` is preserved; `CobolReferenceBuilder` now always extracts qualifiers |
| `reference(GeneralIdentifierContext)` implementation | Task 4 |
| Test cases for `isSuffixMatchedBy` | Task 1 |
| Test cases for `SmojolSymbolTable` integration | Task 4 |
| Three call site updates | `CobolReferenceBuilder` (Task 5); `GeneralIdentifierVisitor` / `CobolExpressionBuilder` create `VariableExpression` (string name for lazy resolution) — these do NOT perform `CobolDataStructure` lookup, so no change needed there |
| No null returns | `NullDataStructure` returned from `CobolDataStructure.reference`; `NullSymbolReference.INSTANCE` from `SmojolSymbolTable.reference` |
| No mutation in resolution path | `allPaths` uses streams; `resolveQualified` uses stream filtering; `isSuffixMatchedBy` is recursive |

### Placeholder scan
None found.

### Type consistency
- `QualifiedName.of(String)` and `QualifiedName.of(String, List<String>)` — used consistently throughout
- `CobolDataStructure.reference(QualifiedName)` — called in Task 3 tests and Task 5 implementation
- `SmojolSymbolTable.resolveQualified(QualifiedName)` — private, called from `reference(GeneralIdentifierContext)`
- `NullSymbolReference.INSTANCE` — used in Task 4 `SmojolSymbolTable`
- `CobolDataStructureTestFactory.leaf/node` — used identically in Tasks 3, 4, 5 tests
