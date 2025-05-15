package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.mojo.algorithms.domain.TranspilerNode;

import java.util.List;

public class TranspilerCloneOperation {
    public static TranspilerNode clone(TranspilerNode original, List<TranspilerNode> children) {
        return switch (original) {
            case AddNode n -> new AddNode(n.getLhs(), n.getRhs());
            case SubtractNode n -> new SubtractNode(n.getMinuend(), n.getSubtrahend());
            case MultiplyNode n -> new MultiplyNode(n.getLhs(), n.getRhs());
            case DivideNode n -> new DivideNode(n.getDividend(), n.getDivisor());
            case ExponentNode n -> new ExponentNode(n.getBasis(), n.getExponent());
            case NegativeNode n -> new NegativeNode(n.getExpression());
            case EqualToNode n -> new EqualToNode(n.getLhs(), n.getRhs());
            case GreaterThanNode n -> new GreaterThanNode(n.getLhs(), n.getRhs());
            case GreaterThanOrEqualToNode n -> new GreaterThanOrEqualToNode(n.getLhs(), n.getRhs());
            case LessThanNode n -> new LessThanNode(n.getLhs(), n.getRhs());
            case LessThanOrEqualToNode n -> new LessThanOrEqualToNode(n.getLhs(), n.getRhs());
            case PrimitiveValueTranspilerNode n -> new PrimitiveValueTranspilerNode(n.getValue());
            case SymbolReferenceNode n -> new SymbolReferenceNode(n.getName());
            case CallFunctionTranspilerNode n -> new CallFunctionTranspilerNode(n.getFunctionName(), n.getArguments());
            case IndexReferenceNode n -> new IndexReferenceNode(n.getRoot(), n.getIndexes());
            case NotTranspilerNode n -> new NotTranspilerNode(n);
            case AndTranspilerNode n -> new AndTranspilerNode(n.getLhs(), n.getRhs());
            case OrTranspilerNode n -> new OrTranspilerNode(n.getLhs(), n.getRhs());
            case PlaceholderTranspilerNode n -> new PlaceholderTranspilerNode(n.getS());
            case ValueOfNode n -> new ValueOfNode(n.getExpression());
            case NestedConditionNode n -> new NestedConditionNode(n.getExpression());
            case NextLocationNode n -> new NextLocationNode();
            case NamedLocationNode n -> new NamedLocationNode(n.getName());
            case ExitTranspilerNode n -> new ExitTranspilerNode();
            case ExitIterationScopeLocationNode n -> new ExitIterationScopeLocationNode();
            case ProgramTerminalLocationNode n -> new ProgramTerminalLocationNode();
            case PrintTranspilerNode n -> new PrintTranspilerNode(n.getOperands());
            case SetTranspilerNode n -> new SetTranspilerNode(n.getSource(), n.getDestination());
            case LabelledTranspilerCodeBlockNode n ->
                    new LabelledTranspilerCodeBlockNode(n.getName(), n.astChildren(), ImmutableMap.of());
            case DetachedTranspilerCodeBlockNode n -> new DetachedTranspilerCodeBlockNode(children);
            case TranspilerCodeBlockNode n -> new TranspilerCodeBlockNode(children, ImmutableMap.of());
            case IfTranspilerNode n ->
                    new IfTranspilerNode(n.getCondition(), ImmutableList.of(n.getIfThenBlock(), n.getIfElseBlock()));
            case ListIterationTranspilerNode n -> new ListIterationTranspilerNode(n.getIterable(), children.getFirst());
            case JumpTranspilerNode n -> new JumpTranspilerNode(n.getStart(), n.getEnd());
            case JumpIfTranspilerNode n -> new JumpIfTranspilerNode(n.getDestination(), n.getCondition());
            default -> throw new AssertionError("Invalid node type: " + original);
        };
    }
}
