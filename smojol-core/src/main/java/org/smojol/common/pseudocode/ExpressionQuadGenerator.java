package org.smojol.common.pseudocode;

import lombok.Getter;
import org.smojol.common.vm.expression.*;

public class ExpressionQuadGenerator {
    @Getter
    private final QuadSequence quads;
    private final SmojolSymbolTable symbolTable;
    private final SymbolReferenceBuilder symbolReferenceBuilder;

    public ExpressionQuadGenerator(SmojolSymbolTable symbolTable, SymbolReferenceBuilder symbolReferenceBuilder) {
        this.quads = new QuadSequence(symbolTable);
        this.symbolTable = symbolTable;
        this.symbolReferenceBuilder = symbolReferenceBuilder;
    }

    private AbstractOperator operator(BinaryCobolOperatorExpression expression) {
        return switch (expression) {
            case AdditionExpression e -> AbstractOperator.ADDITION;
            case SubtractionExpression e -> AbstractOperator.SUBTRACTION;
            case MultiplyExpression e -> AbstractOperator.MULTIPLICATION;
            case DivideExpression e -> AbstractOperator.DIVISION;
            default -> throw new IllegalStateException("Unexpected value: " + expression);
        };
    }

    public SymbolReference build(CobolExpression expression) {
        final SymbolReference symbolReference = symbolReferenceBuilder.expressionSymbolReference(expression);
        switch (expression) {
            case PrimitiveCobolExpression primitive ->
                    quads.add(new InstructionQuad(symbolReference, AbstractOperator.ASSIGNMENT, symbolReferenceBuilder.staticReference(primitive.data()), symbolReferenceBuilder.nullReference()));
            case BinaryCobolOperatorExpression binaryExpre ->
                    quads.add(new InstructionQuad(symbolReference, operator(binaryExpre), build(binaryExpre.getLhs()), build(binaryExpre.getRhs())));
            case ExponentExpression exponentExpr ->
                    quads.add(new InstructionQuad(symbolReference, AbstractOperator.EXPONENT, build(exponentExpr.getBasis()), build(exponentExpr.getExponent())));
            case NegativeExpression negativeExpr ->
                    quads.add(new InstructionQuad(symbolReference, AbstractOperator.NEGATION, build(negativeExpr.getExpression()), symbolReferenceBuilder.nullReference()));
            case NestedConditionExpression nestedExpr ->
                    quads.add(new InstructionQuad(symbolReference, AbstractOperator.NESTED, build(nestedExpr.getExpression()), symbolReferenceBuilder.nullReference()));
            case SimpleConditionExpression simpleConditionExpr ->
                    quads.add(simpleConditionQuad(simpleConditionExpr, symbolReference));
            case BinaryCobolLogicExpression binaryCobolLogicExpr ->
                    quads.add(binaryLogicQuad(binaryCobolLogicExpr, symbolReference));
            case NotExpression notExpr ->
                    quads.add(new InstructionQuad(symbolReference, AbstractOperator.NOT, build(notExpr.getExpression()), symbolReferenceBuilder.nullReference()));
            case null, default -> throw new RuntimeException("Unhandled expression: " + expression);
        }

        return symbolReference;
    }

    private InstructionQuad binaryLogicQuad(BinaryCobolLogicExpression binaryCobolLogicExpr, SymbolReference symbolReference) {
        return switch (binaryCobolLogicExpr) {
            case AndExpression a ->
                    new InstructionQuad(symbolReference, AbstractOperator.AND, build(a.getLhs()), build(a.getRhs()));
            case OrExpression o ->
                    new InstructionQuad(symbolReference, AbstractOperator.OR, build(o.getLhs()), build(o.getRhs()));
            default -> throw new IllegalStateException("Unexpected value: " + binaryCobolLogicExpr);
        };
    }

    private InstructionQuad simpleConditionQuad(SimpleConditionExpression simpleConditionExpr, SymbolReference symbolReference) {
        if (simpleConditionExpr.isStandalone()) {
            // TODO: Reference data structure for assignment and comparison
            return new InstructionQuad(symbolReference, AbstractOperator.EQUAL_TO, symbolReferenceBuilder.nullReference(), build(simpleConditionExpr.getLhs()));
        } else {
            return new InstructionQuad(symbolReference, AbstractOperator.EQUAL_TO, build(simpleConditionExpr.getLhs()), build(simpleConditionExpr.getComparison().getRhs()));
        }
    }

}
