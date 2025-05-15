package org.smojol.toolkit.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.transpiler.*;
import org.smojol.common.vm.expression.*;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.logging.Logger;

public class TranspilerExpressionBuilder {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(TranspilerExpressionBuilder.class.getName());
    private final CobolDataStructure dataStructures;

    public TranspilerExpressionBuilder(CobolDataStructure dataStructures) {
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
        else if (expression instanceof PrimitiveCobolExpression e) return new PrimitiveValueTranspilerNode(e.data());
        else if (expression instanceof FunctionCallExpression e)
            return new CallFunctionTranspilerNode(e.getFunctionName(), e.getArguments().stream().map(this::build).toList());
        else if (expression instanceof NotExpression e) return new NotTranspilerNode(build(e.getExpression()));
        else if (expression instanceof AndExpression e) return new AndTranspilerNode(build(e.getLhs()), build(e.getRhs()));
        else if (expression instanceof OrExpression e) return new OrTranspilerNode(build(e.getLhs()), build(e.getRhs()));
        else if (expression instanceof NestedConditionExpression e) return new NestedConditionNode(build(e.getExpression()));
        else if (expression instanceof SimpleConditionExpression e) {
            if (e.getComparison() == null) return explicitCondition(e.getLhs(), dataStructures);
            return operator(e.getComparison().getRelationalOperation(), build(e.getLhs()), build(e.getComparison().getRhs()));
        } else if (expression instanceof SpecialRegisterExpression e)
            return new CallFunctionTranspilerNode(e.getFunctionCall().getFunctionName(), e.getFunctionCall().getArguments().stream().map(this::build).toList());
        else if (expression instanceof NullCobolExpression e) return new NullTranspilerNode();
        else if (expression instanceof IsNumericCondition e) return new CallFunctionTranspilerNode("isNumeric", ImmutableList.of(build(e.getExpression())));
        else if (expression instanceof IsAlphabeticCondition e) return new CallFunctionTranspilerNode("isAlphanumeric", ImmutableList.of(build(e.getExpression())));
        // TODO: IDMS expressions not supported yet
        throw new UnsupportedOperationException("Unknown expression type: " + expression);
    }

    private TranspilerNode explicitCondition(CobolExpression conditionalConstant, CobolDataStructure root) {
        LOGGER.finest("Resolving conditional constant: " + conditionalConstant.description());
        if (conditionalConstant instanceof IdmsExpression) return new CallFunctionTranspilerNode("idms_placeholder_function", ImmutableList.of(new SymbolReferenceNode(conditionalConstant.description())));
        CobolDataStructure range = root.reference(((VariableExpression) conditionalConstant).getName());
        CobolDataStructure actualVariable = range.parent();
        return new CallFunctionTranspilerNode("isInRange", ImmutableList.of(new SymbolReferenceNode(actualVariable.name()), new SymbolReferenceNode(range.name())));
    }

    public static TranspilerComparisonOperator operator(ComparisonOperator comparison, TranspilerNode lhs, TranspilerNode rhs) {
        if (comparison == RelationalOperation.EQUAL) return new EqualToNode(lhs, rhs);
        else if (comparison == RelationalOperation.NOT_EQUAL) return new NotEqualToNode(lhs, rhs);
        else if (comparison == RelationalOperation.GREATER_THAN) return new GreaterThanNode(lhs, rhs);
        else if (comparison == RelationalOperation.GREATER_THAN_OR_EQUAL) return new GreaterThanOrEqualToNode(lhs, rhs);
        else if (comparison == RelationalOperation.LESS_THAN) return new LessThanNode(lhs, rhs);
        else if (comparison == RelationalOperation.LESS_THAN_OR_EQUAL) return new LessThanOrEqualToNode(lhs, rhs);

        throw new UnsupportedOperationException("This is not a valid comparison operator: " + comparison);
    }

}
