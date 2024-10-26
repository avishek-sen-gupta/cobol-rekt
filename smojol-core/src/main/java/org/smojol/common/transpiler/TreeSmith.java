package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.list.CarCdr;
import org.smojol.common.navigation.TreeNodeParentMapper;
import org.smojol.common.navigation.TreeTraversal;
import org.smojol.common.vm.expression.ConditionTestTime;
import org.smojol.common.vm.type.TypedRecord;

import java.util.List;
import java.util.Optional;

public class TreeSmith {
    private final TranspilerNode root;
    private final TreeNodeParentMapper parentMapper;
    private final TranspilerNodeOrderingVisitor orderVisitor;

    public TreeSmith(TranspilerNode root) {
        this.root = root;
        parentMapper = new TreeNodeParentMapper(root);
        orderVisitor = new TranspilerNodeOrderingVisitor();
        new TreeTraversal<TranspilerNode>().run(root, orderVisitor);
    }

    public TranspilerNode escapeScopeOnce(TranspilerNode jumpNode) {
        TranspilerNode currentScope = parentMapper.parentOf(jumpNode);
        TranspilerNode condition = new PrimitiveValueTranspilerNode(TypedRecord.TRUE);
        List<TranspilerNode> everythingAfter = currentScope.everythingAfter(jumpNode);
        TranspilerNode newIf = new IfTranspilerNode(new NotTranspilerNode(condition), new TranspilerCodeBlockNode(everythingAfter));
        SetTranspilerNode setCondition = new SetTranspilerNode(condition, new SymbolReferenceNode("SOME"));
        boolean replaced = currentScope.replaceToEnd(jumpNode, ImmutableList.of(setCondition, newIf));
        if (!replaced) return jumpNode;
        TranspilerNode jumpIfTranspilerNode = switch (jumpNode) {
            case JumpTranspilerNode j -> new JumpIfTranspilerNode(j.getStart(), condition);
            case JumpIfTranspilerNode k -> new JumpIfTranspilerNode(k.getDestination(), condition);
            default ->
                    throw new IllegalStateException(String.format("Unexpected node of type %s. Description is:\n%s", jumpNode.getClass(), jumpNode.description()));
        };
        TreeNodeLocation graftLocation = parentMapper.parentGraftLocation(currentScope);
        boolean couldGraft = graftLocation.parentScope().addAfter(graftLocation.location(), ImmutableList.of(jumpIfTranspilerNode));
        if (!couldGraft) return jumpNode;
        parentMapper.update(graftLocation.parentScope());
        return jumpIfTranspilerNode;
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

    public boolean eliminateBackJump(JumpTranspilerNode jumpNode) {
        TranspilerNode parent = parentMapper.parentOf(jumpNode);
        Optional<JumpIfTranspilerNode> replacingJumpIf = replaceJumpWithJumpIf(jumpNode, parent);
        if (replacingJumpIf.isEmpty()) return false;
        parentMapper.update(parent);
        return eliminateBackJump(replacingJumpIf.get());
    }

    public boolean eliminateForwardJump(JumpTranspilerNode jumpNode) {
        TranspilerNode parent = parentMapper.parentOf(jumpNode);
        Optional<JumpIfTranspilerNode> replacingJumpIf = replaceJumpWithJumpIf(jumpNode, parent);
        if (replacingJumpIf.isEmpty()) return false;
        parentMapper.update(parent);
        return eliminateForwardJump(replacingJumpIf.get());
    }

    public boolean eliminateForwardJump(JumpIfTranspilerNode jumpNode) {
        TranspilerNode parent = parentMapper.parentOf(jumpNode);
        Optional<TranspilerNode> maybeJumpTarget = parent.findOne(n -> n instanceof LabelledTranspilerCodeBlockNode l
                && l.getName().equals(((NamedLocationNode) jumpNode.getDestination()).getName()));
        if (maybeJumpTarget.isEmpty()) return false;
        TranspilerNode jumpTarget = maybeJumpTarget.get();
        List<TranspilerNode> range = parent.range(jumpNode, jumpTarget);
        List<TranspilerNode> ifBody = CarCdr.tail(CarCdr.init(range));
        TranspilerNode ifNode = new IfTranspilerNode(new NotTranspilerNode(jumpNode.getCondition()), new TranspilerCodeBlockNode(ifBody));
        boolean couldGraft = parent.replaceRangeToExclusive(ImmutablePair.of(jumpNode, jumpTarget), ImmutableList.of(ifNode));
        parentMapper.update(parent);
        return couldGraft;
    }

    Optional<JumpIfTranspilerNode> replaceJumpWithJumpIf(JumpTranspilerNode jumpNode, TranspilerNode parent) {
        SetTranspilerNode newSet = new SetTranspilerNode(new SymbolReferenceNode("EFGH"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        JumpIfTranspilerNode replacingJumpIf = new JumpIfTranspilerNode(jumpNode.getStart(), new ValueOfNode(new SymbolReferenceNode("EFGH")));
        return parent.replace(jumpNode, ImmutableList.of(newSet, replacingJumpIf)) ? Optional.of(replacingJumpIf) : Optional.empty();
    }

    public Pair<TranspilerNode, Boolean> escapeScope(TranspilerNode node) {
        LocationNode destinationBlockLabel = destination(node);

        if (destinationBlockLabel == LocationNode.NULL) return ImmutablePair.of(node, false);
        NamedLocationNode namedLocation = (NamedLocationNode) destinationBlockLabel;
        Optional<TranspilerNode> first = root.findAllRecursive(n -> n instanceof LabelledTranspilerCodeBlockNode l && namedLocation.getName().equals(l.getName())).stream().findFirst();
        if (first.isEmpty()) return ImmutablePair.of(node, false);
        int destinationLevel = level(first.get());
        TranspilerNode current = node;
        while (level(current) != destinationLevel) {
            TranspilerNode updated = escapeScopeOnce(current);
            if (updated == current) return ImmutablePair.of(current, false);
            current = updated;
        }

        return ImmutablePair.of(current, true);
    }

    private static LocationNode destination(TranspilerNode node) {
        return switch (node) {
            case JumpTranspilerNode j -> j.getStart();
            case JumpIfTranspilerNode k -> k.getDestination();
            default -> LocationNode.NULL;
        };
    }

    private int level(TranspilerNode node) {
        TranspilerNode parent = parentMapper.parentOf(node);
        int level = 0;
        while (parent != root) {
            level++;
            parent = parentMapper.parentOf(parent);
        }
        return level;
    }

    public boolean eliminateForwardJump(TranspilerNode node) {
        return switch (node) {
            case JumpTranspilerNode j -> eliminateForwardJump(j);
            case JumpIfTranspilerNode k -> eliminateForwardJump(k);
            default -> throw new IllegalStateException("Unexpected value: " + node);
        };
    }

    public JumpType jumpType(TranspilerNode node) {
        LocationNode destination = destination(node);
        if (destination == LocationNode.NULL) throw new RuntimeException("Invalid location: " + node);

        NamedLocationNode namedLocation = (NamedLocationNode) destination;
        List<TranspilerNode> allMatchingBlocks = root.findAllRecursive(n -> n instanceof LabelledTranspilerCodeBlockNode l && namedLocation.getName().equals(l.getName()));
        if (allMatchingBlocks.isEmpty()) throw new RuntimeException("Invalid location: " + node);
        return orderVisitor.order(node, allMatchingBlocks.getFirst());
    }
}
