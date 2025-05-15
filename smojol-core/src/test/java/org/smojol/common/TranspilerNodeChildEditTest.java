package org.smojol.common;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.mojo.algorithms.domain.FlowNodeType;
import com.mojo.algorithms.domain.TranspilerNode;
import com.mojo.algorithms.domain.TypedRecord;
import com.mojo.algorithms.transpiler.*;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

public class TranspilerNodeChildEditTest {
    @Test
    public void canReplaceExistingNodeWithMultipleNodes() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("XYZ", 60);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2));
        assertTrue(parent.replace(set1, ImmutableList.of(set3, set4)));
        assertEquals(ImmutableList.of(set3, set4, set2), parent.astChildren());
    }

    @Test
    public void cannotReplaceNonExistentNodes() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(set1));
        assertFalse(parent.replace(set2, ImmutableList.of(set3)));
        assertEquals(ImmutableList.of(set1), parent.astChildren());
    }

    @Test
    public void cannotTouchEmptyChildrenList() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of());
        assertFalse(parent.replace(set1, ImmutableList.of(set2)));
        assertEquals(ImmutableList.of(), parent.astChildren());
    }

    @Test
    public void cannotInsertAfterSpecifiedChildIfNoChildren() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of());
        assertFalse(parent.addAfter(set1, ImmutableList.of(set2)));
        assertEquals(ImmutableList.of(), parent.astChildren());
    }

    @Test
    public void canInsertAfterSpecifiedChild() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("XYZ", 60);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2));
        assertTrue(parent.addAfter(set1, ImmutableList.of(set3, set4)));
        assertEquals(ImmutableList.of(set1, set3, set4, set2), parent.astChildren());
    }

    @Test
    public void canInsertAfterSpecifiedChildIfChildIsLastElement() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("XYZ", 60);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2));
        assertTrue(parent.addAfter(set2, ImmutableList.of(set3, set4)));
        assertEquals(ImmutableList.of(set1, set2, set3, set4), parent.astChildren());
    }

    @Test
    public void canFindAllNodesAfterSpecificChild() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("XYZ", 60);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2, set3, set4));

        assertEquals(ImmutableList.of(set3, set4), parent.everythingAfter(set2));
    }

    @Test
    public void canFindNoNodesAfterLastChild() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("XYZ", 60);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2, set3, set4));

        assertTrue(parent.everythingAfter(set4).isEmpty());
    }

    @Test
    public void canFindNoNodesAfterInvalidChild() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("XYZ", 60);
        TranspilerNode set5 = set("KLM", 70);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2, set3, set4));

        assertTrue(parent.everythingAfter(set5).isEmpty());
    }

    @Test
    public void canReplaceFromChildToEnd() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode set5 = set("NOP", 80);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2, set3));

        assertTrue(parent.replaceToEnd(set3, ImmutableList.of(set4, set5)));
        assertEquals(ImmutableList.of(set1, set2, set4, set5), parent.astChildren());
    }

    @Test
    public void cannotReplaceFromChildToEndIfChildDoesNotExist() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode set5 = set("NOP", 80);
        TranspilerNode set6 = set("RST", 90);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2, set3));

        assertFalse(parent.replaceToEnd(set6, ImmutableList.of(set4, set5)));
        assertEquals(ImmutableList.of(set1, set2, set3), parent.astChildren());
    }

    @Test
    public void canReplaceRangeOfChildrenWithAnotherRangeToInclusiveOfNodes() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode set5 = set("NOP", 80);
        TranspilerNode set6 = set("RST", 90);
        TranspilerNode set7 = set("RST", 100);
        TranspilerNode set8 = set("RST", 110);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2, set3, set4));

        assertTrue(parent.replaceRangeToInclusive(ImmutablePair.of(set2, set3), ImmutableList.of(set6, set7, set8)));
        assertEquals(ImmutableList.of(set1, set6, set7, set8, set4), parent.astChildren());
    }

    @Test
    public void cannotReplaceEmptyRangeOfChildrenWithAnotherRangeOfNodes() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode set5 = set("NOP", 80);
        TranspilerNode set6 = set("RST", 90);
        TranspilerNode set7 = set("RST", 100);
        TranspilerNode set8 = set("RST", 110);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2, set3, set4));

        assertFalse(parent.replaceRangeToInclusive(ImmutablePair.of(set5, set5), ImmutableList.of(set6, set7, set8)));
        assertEquals(ImmutableList.of(set1, set2, set3, set4), parent.astChildren());
    }

    @Test
    public void deletesRangeIfSubstitutedWithEmptyListOfNodes() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2, set3, set4));

        assertTrue(parent.replaceRangeToInclusive(ImmutablePair.of(set2, set3), ImmutableList.of()));
        assertEquals(ImmutableList.of(set1, set4), parent.astChildren());
    }

    @Test
    public void canFindRangeOfStatements() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode set5 = set("NOP", 80);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2, set3, set4, set5));

        assertEquals(ImmutableList.of(set2, set3, set4), parent.range(set2, set4));
    }

    @Test
    public void canFindLabelledCodeBlockGivenName() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode block = new LabelledTranspilerCodeBlockNode("SOME_BLOCK", ImmutableList.of(set1, set2), ImmutableMap.of("type", FlowNodeType.PARAGRAPH));
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(block, set3, set4));

        Optional<TranspilerNode> one = parent.findOne(n -> n instanceof LabelledTranspilerCodeBlockNode l && "SOME_BLOCK".equals(l.getName()));
        assertTrue(one.isPresent());
        assertEquals(block, one.get());
    }

    private static SetTranspilerNode set(String variable, int value) {
        return new SetTranspilerNode(new SymbolReferenceNode(variable), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(value)));
    }
}
