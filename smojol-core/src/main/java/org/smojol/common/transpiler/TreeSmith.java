package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.smojol.common.list.CarCdr;
import org.smojol.common.navigation.TreeNodeParentMapper;
import org.smojol.common.vm.expression.ConditionTestTime;
import org.smojol.common.vm.type.TypedRecord;

import java.util.List;
import java.util.Optional;

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
            default ->
                    throw new IllegalStateException(String.format("Unexpected node of type %s. Description is:\n%s", jumpNode.getClass(), jumpNode.description()));
        };
        TreeNodeLocation graftLocation = parentMapper.parentGraftLocation(parentMapper.parentOf(jumpNode));
        boolean couldGraft = graftLocation.parentScope().addAfter(graftLocation.location(), ImmutableList.of(jumpIfTranspilerNode));
        if (!couldGraft) return false;
        parentMapper.update(graftLocation.parentScope());
        return true;
    }

    public boolean eliminateBackJump(JumpIfTranspilerNode jumpNode) {
        TranspilerNode parent = parentMapper.parentOf(jumpNode);
        TranspilerNode from = parent.findOne(n -> n instanceof LabelledTranspilerCodeBlockNode l && "SOME_BLOCK".equals(l.getName())).get();
        TranspilerCodeBlockNode newScope = new TranspilerCodeBlockNode(CarCdr.init(parent.range(from, jumpNode)));
        TranspilerLoop loop = new TranspilerLoop(new SymbolReferenceNode("ABC"), new NullTranspilerNode(), new NullTranspilerNode(),
                jumpNode.getCondition(), new NullTranspilerNode(), ConditionTestTime.AFTER, newScope);

        boolean couldGraft = parent.replaceRangeToInclusive(ImmutablePair.of(from, jumpNode), ImmutableList.of(loop));
        parentMapper.update(parent);
        return couldGraft;
    }

    public boolean eliminateForwardJump(JumpIfTranspilerNode jumpNode) {
        TranspilerNode parent = parentMapper.parentOf(jumpNode);
        Optional<TranspilerNode> maybeJumpTarget = parent.findOne(n -> n instanceof LabelledTranspilerCodeBlockNode l
                && l.getName().equals(((NamedLocationNode) jumpNode.getDestination()).getName()));
        if (maybeJumpTarget.isEmpty()) return false;
        TranspilerNode jumpTarget = maybeJumpTarget.get();
        List<TranspilerNode> range = parent.range(jumpNode, jumpTarget);
        List<TranspilerNode> ifBody = CarCdr.tail(CarCdr.init(range));
        TranspilerNode ifNode = new IfTranspilerNode(jumpNode.getCondition(), new TranspilerCodeBlockNode(ifBody));
        boolean couldGraft = parent.replaceRangeToExclusive(ImmutablePair.of(jumpNode, jumpTarget), ImmutableList.of(ifNode));
        parentMapper.update(parent);
        return couldGraft;
    }
}
