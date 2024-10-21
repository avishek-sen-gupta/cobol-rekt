package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.vm.type.TypedRecord;

import java.util.List;

public class TreeOperations {
    public static boolean escapeScope(TranspilerNode jumpNode, TranspilerNode currentScope, TranspilerNode graftLocation, TranspilerNode parentScope) {
        TranspilerNode condition = new PrimitiveValueTranspilerNode(TypedRecord.TRUE);
        List<TranspilerNode> everythingAfter = currentScope.everythingAfter(jumpNode);
        TranspilerNode newIf = new IfTranspilerNode(new NotTranspilerNode(condition), new TranspilerCodeBlockNode(everythingAfter));
        SetTranspilerNode setCondition = new SetTranspilerNode(condition, new SymbolReferenceNode("SOME"));
        boolean replaced = currentScope.replaceToEnd(jumpNode, ImmutableList.of(setCondition, newIf));
        if (!replaced) return false;
        TranspilerNode jumpIfTranspilerNode = switch (jumpNode) {
            case JumpTranspilerNode j -> new JumpIfTranspilerNode(j.getStart(), condition);
            case JumpIfTranspilerNode k -> new JumpIfTranspilerNode(k.getDestination(), condition);
            default -> throw new IllegalStateException("Unexpected value: " + jumpNode);
        };
        parentScope.addAfter(graftLocation, ImmutableList.of(jumpIfTranspilerNode));
        return true;
    }
}
