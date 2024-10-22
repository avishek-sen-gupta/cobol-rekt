package org.smojol.common;

import com.google.common.collect.ImmutableList;
import org.junit.jupiter.api.Test;
import org.smojol.common.navigation.ParentBuilderVisitor;
import org.smojol.common.navigation.TreeTraversal;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.smojol.common.transpiler.TreeOperations.escapeScope;

public class TreeOperationsTest {
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
    public void canEscapeArbitraryScopeUsingJumpIf() {
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("abcd"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        TranspilerNode gotoSomeplace = new JumpTranspilerNode(new NamedLocationNode("SOMEPLACE"));
        IfTranspilerNode ifStmt = new IfTranspilerNode(condition, new TranspilerCodeBlockNode(gotoSomeplace));
        TranspilerCodeBlockNode program = new TranspilerCodeBlockNode(ifStmt);
        boolean escaped = escapeScope(gotoSomeplace, ifStmt.getIfThenBlock(), ifStmt, program);
        assertTrue(escaped);
    }

    private static SetTranspilerNode set(String variable, int value) {
        return new SetTranspilerNode(new SymbolReferenceNode(variable), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(value)));
    }
}
