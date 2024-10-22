package org.smojol.common;

import com.google.common.collect.ImmutableList;
import org.junit.jupiter.api.Test;
import org.smojol.common.navigation.ParentBuilderVisitor;
import org.smojol.common.navigation.TreeTraversal;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ParentLinkBuildTest {
    @Test
    public void canBuildParentLinks() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode block = new TranspilerCodeBlockNode(set3);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2, block));

        ParentBuilderVisitor visitor = new ParentBuilderVisitor();
        new TreeTraversal<TranspilerNode>().run(parent, visitor);
        Map<TranspilerNode, TranspilerNode> childToParentMap = visitor.getChildToParentMap();
        assertEquals(parent, childToParentMap.get(set1));
        assertEquals(parent, childToParentMap.get(set2));
        assertEquals(parent, childToParentMap.get(block));
        assertEquals(block, childToParentMap.get(set3));
    }

    private static SetTranspilerNode set(String variable, int value) {
        return new SetTranspilerNode(new SymbolReferenceNode(variable), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(value)));
    }
}
