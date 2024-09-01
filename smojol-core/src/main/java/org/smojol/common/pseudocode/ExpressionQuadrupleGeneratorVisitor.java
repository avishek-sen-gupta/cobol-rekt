package org.smojol.common.pseudocode;

import lombok.Getter;
import org.smojol.common.id.IdProvider;
import org.smojol.common.vm.expression.*;

public class ExpressionQuadrupleGeneratorVisitor implements CobolExpressionVisitor {
    @Getter private final QuadSequence quads;
    private final SmojolSymbolTable symbolTable;
    private final SymbolReferenceBuilder symbolReferenceBuilder;

    public ExpressionQuadrupleGeneratorVisitor(IdProvider idProvider, SmojolSymbolTable symbolTable, SymbolReferenceBuilder symbolReferenceBuilder) {
        this.quads = new QuadSequence(symbolTable);
        this.symbolTable = symbolTable;
        this.symbolReferenceBuilder = symbolReferenceBuilder;
    }

    @Override
    public void visit(CobolExpression expression) {
        switch (expression) {
            case PrimitiveCobolExpression primitive -> quads.add(quad(primitive));
            case BinaryCobolOperatorExpression binaryExpre ->
                    quads.add(new InstructionQuad(variableSymbolReference(binaryExpre), operator(binaryExpre), quads.existingSymbol(binaryExpre.getLhs()), quads.existingSymbol(binaryExpre.getRhs())));
            case ExponentExpression exponentExpr ->
                    quads.add(new InstructionQuad(variableSymbolReference(exponentExpr), AbstractOperator.EXPONENT, quads.existingSymbol(exponentExpr.getBasis()), quads.existingSymbol(exponentExpr.getExponent())));
            case NegativeExpression negativeExpre ->
                    quads.add(new InstructionQuad(variableSymbolReference(negativeExpre), AbstractOperator.NEGATION, quads.existingSymbol(negativeExpre.getExpression()), symbolReferenceBuilder.nullReference()));
            case NestedConditionExpression nestedExpr ->
                    quads.add(new InstructionQuad(variableSymbolReference(nestedExpr), AbstractOperator.NESTED, quads.existingSymbol(nestedExpr.getExpression()), symbolReferenceBuilder.nullReference()));
            case SimpleConditionExpression simpleConditionExpr -> quads.add(simpleConditionQuad(simpleConditionExpr));
            case BinaryCobolLogicExpression binaryCobolLogicExpr -> quads.add(binaryLogicQuad(binaryCobolLogicExpr));
            case NotExpression notExpr ->
                    quads.add(new InstructionQuad(variableSymbolReference(notExpr), AbstractOperator.NOT, quads.existingSymbol(notExpr.getExpression()), symbolReferenceBuilder.nullReference()));
            case null, default -> throw new RuntimeException("Unhandled expression: " + expression);
        }
    }

    private SymbolReference variableSymbolReference(CobolExpression expression) {
//        IntermediateSymbolReference intermediateSymbolReference = new IntermediateSymbolReference(expression, idProvider.next());
        SymbolReference intermediateSymbolReference = symbolReferenceBuilder.intermediateSymbolReference(expression);
        symbolTable.add(intermediateSymbolReference);
        return intermediateSymbolReference;
    }

    private InstructionQuad binaryLogicQuad(BinaryCobolLogicExpression binaryCobolLogicExpr) {
        return switch (binaryCobolLogicExpr) {
            case AndExpression a ->
                    new InstructionQuad(variableSymbolReference(binaryCobolLogicExpr), AbstractOperator.AND, quads.existingSymbol(a.getLhs()), quads.existingSymbol(a.getRhs()));
            case OrExpression o ->
                    new InstructionQuad(variableSymbolReference(binaryCobolLogicExpr), AbstractOperator.OR, quads.existingSymbol(o.getLhs()), quads.existingSymbol(o.getRhs()));
            default -> throw new IllegalStateException("Unexpected value: " + binaryCobolLogicExpr);
        };
    }

    private InstructionQuad simpleConditionQuad(SimpleConditionExpression simpleConditionExpr) {
        if (simpleConditionExpr.isStandalone()) {
            // TODO: Reference data structure for assignment and comparison
            return new InstructionQuad(variableSymbolReference(simpleConditionExpr), AbstractOperator.EQUAL_TO, symbolReferenceBuilder.nullReference(), quads.existingSymbol(simpleConditionExpr.getLhs()));
        } else {
            return new InstructionQuad(variableSymbolReference(simpleConditionExpr), AbstractOperator.EQUAL_TO, quads.existingSymbol(simpleConditionExpr.getLhs()), quads.existingSymbol(simpleConditionExpr.getComparison().getRhs()));
        }
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

    private InstructionQuad quad(PrimitiveCobolExpression primitive) {
//        return new InstructionQuad(variableSymbolReference(primitive), AbstractOperator.ASSIGNMENT, new StaticSymbolReference(primitive.data(), idProvider.next()), symbolReferenceBuilder.nullReference());
        return new InstructionQuad(variableSymbolReference(primitive), AbstractOperator.ASSIGNMENT, symbolReferenceBuilder.staticReference(primitive.data()), symbolReferenceBuilder.nullReference());
    }
}
