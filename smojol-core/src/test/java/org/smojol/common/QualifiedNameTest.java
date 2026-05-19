// smojol-core/src/test/java/org/smojol/common/QualifiedNameTest.java
package org.smojol.common;

import org.junit.jupiter.api.Test;
import org.smojol.common.pseudocode.QualifiedName;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class QualifiedNameTest {

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

    @Test
    public void qualifiedNameMatchesWhenQualifiersFormSuffixSubsequence() {
        QualifiedName qn = QualifiedName.of("FIELD-A", List.of("STRUCT-1"));
        assertTrue(qn.isSuffixMatchedBy(List.of("ROOT", "STRUCT-1", "FIELD-A")));
    }

    @Test
    public void qualifiedNameDoesNotMatchWrongParent() {
        QualifiedName qn = QualifiedName.of("FIELD-A", List.of("STRUCT-2"));
        assertFalse(qn.isSuffixMatchedBy(List.of("ROOT", "STRUCT-1", "FIELD-A")));
    }

    @Test
    public void qualifierCanSkipIntermediateAncestors() {
        QualifiedName qn = QualifiedName.of("FIELD-A", List.of("ROOT"));
        assertTrue(qn.isSuffixMatchedBy(List.of("ROOT", "STRUCT-1", "FIELD-A")));
    }

    @Test
    public void fullQualificationMatchesExactPath() {
        QualifiedName qn = QualifiedName.of("FIELD-A", List.of("STRUCT-1", "ROOT"));
        assertTrue(qn.isSuffixMatchedBy(List.of("ROOT", "STRUCT-1", "FIELD-A")));
    }

    @Test
    public void missingQualifierNameFailsMatch() {
        QualifiedName qn = QualifiedName.of("FIELD-A", List.of("MISSING"));
        assertFalse(qn.isSuffixMatchedBy(List.of("ROOT", "STRUCT-1", "FIELD-A")));
    }

    @Test
    public void bareNameReturnsFirstPart() {
        QualifiedName qn = QualifiedName.of("FIELD-A", List.of("STRUCT-1"));
        assertEquals("FIELD-A", qn.bareName());
    }

    @Test
    public void doesNotCommitToEarlyOccurrenceOfRepeatedLabel() {
        // candidate has FIELD-A twice; only the last one is at the tail
        QualifiedName qn = QualifiedName.of("FIELD-A", List.of("STRUCT-1"));
        assertTrue(qn.isSuffixMatchedBy(List.of("ROOT", "STRUCT-1", "FIELD-A", "FIELD-A")));
    }
}
