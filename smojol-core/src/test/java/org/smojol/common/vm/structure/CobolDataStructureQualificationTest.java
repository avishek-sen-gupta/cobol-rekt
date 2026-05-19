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

    @Test
    public void allPathsIncludesRootAsFirstEntry() {
        var paths = root.allPaths(List.of()).map(Map.Entry::getKey).toList();
        assertEquals(List.of("ROOT"), paths.get(0));
    }

    @Test
    public void allPathsIncludesAllNodes() {
        var paths = root.allPaths(List.of()).map(Map.Entry::getKey).toList();
        assertEquals(6, paths.size());
    }

    @Test
    public void allPathsRecordsFullAncestorChain() {
        var paths = root.allPaths(List.of()).map(Map.Entry::getKey).toList();
        assertTrue(paths.contains(List.of("ROOT", "STRUCT-1", "FIELD-A")));
        assertTrue(paths.contains(List.of("ROOT", "STRUCT-2", "FIELD-A")));
        assertTrue(paths.contains(List.of("ROOT", "STRUCT-2", "FIELD-B")));
    }

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
    public void referenceWithUnknownQualifierReturnsNullDataStructure() {
        CobolDataStructure resolved = root.reference(
            QualifiedName.of("FIELD-A", List.of("MISSING")));
        assertTrue(resolved instanceof NullDataStructure);
    }

    @Test
    public void referenceOfNonExistentNameReturnsNullDataStructure() {
        CobolDataStructure resolved = root.reference(QualifiedName.of("NONEXISTENT"));
        assertTrue(resolved instanceof NullDataStructure);
    }

    @Test
    public void ambiguousBareNameThrowsException() {
        assertThrows(AmbiguousQualifierException.class,
            () -> root.reference(QualifiedName.of("FIELD-A")));
    }
}
