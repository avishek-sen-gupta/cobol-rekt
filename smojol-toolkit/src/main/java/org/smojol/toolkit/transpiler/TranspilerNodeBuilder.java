package org.smojol.toolkit.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.expression.*;
import org.smojol.common.vm.structure.CobolDataStructure;

public class TranspilerNodeBuilder {
    private final CobolDataStructure dataStructures;

    public TranspilerNodeBuilder(CobolDataStructure dataStructures) {
        this.dataStructures = dataStructures;
    }

    public TranspilerNode build(CobolExpression expression) {
        if (expression instanceof VariableExpression e) return new SymbolReferenceNode(e.getName());
        else if (expression instanceof TableCallExpression e)
            return new IndexReferenceNode(new SymbolReferenceNode(e.getVariableExpression().getName()), e.getIndexes().stream().map(this::build).toList());
        else if (expression instanceof AdditionExpression e) return new AddNode(build(e.getLhs()), build(e.getRhs()));
        else if (expression instanceof SubtractionExpression e)
            return new SubtractNode(build(e.getLhs()), build(e.getRhs()));
        else if (expression instanceof MultiplyExpression e)
            return new MultiplyNode(build(e.getLhs()), build(e.getRhs()));
        else if (expression instanceof DivideExpression e) return new DivideNode(build(e.getLhs()), build(e.getRhs()));
        else if (expression instanceof ExponentExpression e)
            return new ExponentNode(build(e.getBasis()), build(e.getBasis()));
        else if (expression instanceof NegativeExpression e) return new NegativeNode(build(e.getExpression()));
        else if (expression instanceof PrimitiveCobolExpression e) return new PrimitiveValueNode(e.data());
        else if (expression instanceof FunctionCallExpression e)
            return new FunctionCallNode(e.getFunctionName(), e.getArguments().stream().map(this::build).toList());
        else if (expression instanceof NotExpression e) return new NotNode(build(e.getExpression()));
        else if (expression instanceof AndExpression e) return new AndNode(build(e.getLhs()), build(e.getRhs()));
        else if (expression instanceof OrExpression e) return new OrNode(build(e.getLhs()), build(e.getRhs()));
        else if (expression instanceof NestedConditionExpression e) return new NestedNode(build(e.getExpression()));
        else if (expression instanceof SimpleConditionExpression e) {
            if (e.getComparison() == null) return explicitCondition(e, dataStructures);
            return TranspilerComparisonOperator.operator(e.getComparison().getRelationalOperation(), build(e.getLhs()), build(e.getComparison().getRhs()));
        } else if (expression instanceof SpecialRegisterExpression e)
            return new FunctionCallNode(e.getFunctionCall().getFunctionName(), e.getFunctionCall().getArguments().stream().map(this::build).toList());
        // TODO: IDMS expressions not supported yet
        throw new UnsupportedOperationException("Unknown expression type: " + expression.getClass());
    }

    private TranspilerNode explicitCondition(CobolExpression conditionalConstant, CobolDataStructure root) {
        CobolDataStructure range = root.reference(((VariableExpression) conditionalConstant).getName());
        CobolDataStructure actualVariable = range.parent();
        return new FunctionCallNode("isInRange", ImmutableList.of(new SymbolReferenceNode(actualVariable.name()), new SymbolReferenceNode(range.name())));
    }
}
