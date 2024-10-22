package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.navigation.TreeNodeParentMapper;
import org.smojol.common.vm.type.TypedRecord;

import java.util.List;

public class TreeSmith {
    private final TranspilerNode root;
    private final TreeNodeParentMapper parentMapper;

    public TreeSmith(TranspilerNode root) {
        this.root = root;
        parentMapper = new TreeNodeParentMapper(root);
    }

    public boolean escapeScope(TranspilerNode jumpNode, TranspilerNode currentScope) {
        TranspilerNode condition = new PrimitiveValueTranspilerNode(TypedRecord.TRUE);
        List<TranspilerNode> everythingAfter = currentScope.everythingAfter(jumpNode);
        TranspilerNode newIf = new IfTranspilerNode(new NotTranspilerNode(condition), new TranspilerCodeBlockNode(everythingAfter));
        SetTranspilerNode setCondition = new SetTranspilerNode(condition, new SymbolReferenceNode("SOME"));
        boolean replaced = currentScope.replaceToEnd(jumpNode, ImmutableList.of(setCondition, newIf));
        if (!replaced) return false;
        TranspilerNode jumpIfTranspilerNode = switch (jumpNode) {
            case JumpTranspilerNode j -> new JumpIfTranspilerNode(j.getStart(), condition);
            case JumpIfTranspilerNode k -> new JumpIfTranspilerNode(k.getDestination(), condition);
            default -> throw new IllegalStateException(String.format("Unexpected node of type %s. Description is:\n%s", jumpNode.getClass(), jumpNode.description()));
        };
        TreeNodeLocation graftLocation = parentMapper.parentGraftLocation(parentMapper.parentOf(jumpNode));
        graftLocation.parentScope().addAfter(graftLocation.location(), ImmutableList.of(jumpIfTranspilerNode));
        parentMapper.update(graftLocation.parentScope());
        return true;
    }
}
