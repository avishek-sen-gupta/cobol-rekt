package org.smojol.common.pseudocode;

import lombok.Getter;
import org.smojol.common.id.IdProvider;
import org.smojol.common.vm.expression.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class QuadrupleGeneratorVisitor implements CobolExpressionVisitor {
    private final IdProvider idProvider;
    @Getter
    private final List<InstructionQuad> quads = new ArrayList<>();

    public QuadrupleGeneratorVisitor(IdProvider idProvider) {
        this.idProvider = idProvider;
    }

    @Override
    public void visit(CobolExpression expression) {
        if (expression instanceof PrimitiveCobolExpression primitive) {
            quads.add(quad(primitive));
        } else if (expression instanceof BinaryCobolOperatorExpression binaryExpre) {
            quads.add(new InstructionQuad(variableSymbolReference(binaryExpre), operator(binaryExpre), existingSymbol(binaryExpre.getLhs()), existingSymbol(binaryExpre.getRhs())));
        } else if (expression instanceof ExponentExpression exponentExpr) {
            quads.add(new InstructionQuad(variableSymbolReference(exponentExpr), AbstractOperator.EXPONENT, existingSymbol(exponentExpr.getBasis()), existingSymbol(exponentExpr.getExponent())));
        } else if (expression instanceof NegativeExpression negativeExpre) {
            quads.add(new InstructionQuad(variableSymbolReference(negativeExpre), AbstractOperator.NEGATION, existingSymbol(negativeExpre.getExpression()), new NullSymbolReference()));
        } else if (expression instanceof NestedConditionExpression nestedExpr) {
            quads.add(new InstructionQuad(variableSymbolReference(nestedExpr), AbstractOperator.NESTED, existingSymbol(nestedExpr.getExpression()), new NullSymbolReference()));
        } else if (expression instanceof SimpleConditionExpression simpleConditionExpr) {
            quads.add(simpleConditionQuad(simpleConditionExpr));
        } else if (expression instanceof BinaryCobolLogicExpression binaryCobolLogicExpr) {
            quads.add(binaryLogicQuad(binaryCobolLogicExpr));
        } else if (expression instanceof NotExpression notExpr) {
            quads.add(new InstructionQuad(variableSymbolReference(notExpr), AbstractOperator.NOT, existingSymbol(notExpr.getExpression()), new NullSymbolReference()));
        } else
            throw new RuntimeException("Unhandled expression: " + expression);
    }

    private VariableSymbolReference variableSymbolReference(CobolExpression expression) {
        return new VariableSymbolReference(expression, idProvider.next());
    }

    private VariableSymbolReference variableSymbolReferenceSearchSpec(CobolExpression expression) {
        return new VariableSymbolReference(expression, "-1");
    }

    private InstructionQuad binaryLogicQuad(BinaryCobolLogicExpression binaryCobolLogicExpr) {
        return switch (binaryCobolLogicExpr) {
            case AndExpression a ->
                    new InstructionQuad(variableSymbolReference(binaryCobolLogicExpr), AbstractOperator.AND, existingSymbol(a.getLhs()), existingSymbol(a.getRhs()));
            case OrExpression o ->
                    new InstructionQuad(variableSymbolReference(binaryCobolLogicExpr), AbstractOperator.OR, existingSymbol(o.getLhs()), existingSymbol(o.getRhs()));
            default -> throw new IllegalStateException("Unexpected value: " + binaryCobolLogicExpr);
        };
    }

    private InstructionQuad simpleConditionQuad(SimpleConditionExpression simpleConditionExpr) {
        if (simpleConditionExpr.isStandalone()) {
            // TODO: Reference data structure for assignment and comparison
            return new InstructionQuad(variableSymbolReference(simpleConditionExpr), AbstractOperator.EQUAL_TO, new NullSymbolReference(), existingSymbol(simpleConditionExpr.getLhs()));
        } else {
            return new InstructionQuad(variableSymbolReference(simpleConditionExpr), AbstractOperator.EQUAL_TO, existingSymbol(simpleConditionExpr.getLhs()), existingSymbol(simpleConditionExpr.getComparison().getRhs()));
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
//        Optional<InstructionQuad> any = existingQuad(primitive);
//        if (any.isPresent()) return any.get();
        //        quads.add(newQuadruple);
        return new InstructionQuad(variableSymbolReference(primitive), AbstractOperator.ASSIGNMENT, new StaticSymbolReference(primitive.data()), new NullSymbolReference());
    }

    private Optional<InstructionQuad> existingQuad(PrimitiveCobolExpression primitive) {
        Optional<InstructionQuad> any = quads.stream().filter(q -> q.operator() == AbstractOperator.ASSIGNMENT && q.lhs().equals(variableSymbolReference(primitive))).findAny();
        return any;
    }

    private SymbolReference existingSymbol(CobolExpression expr) {
        Optional<InstructionQuad> any = quads.stream().filter(q -> q.result().equals(variableSymbolReferenceSearchSpec(expr))).findAny();
        if (any.isEmpty()) throw new UnresolvedSymbolReferenceException(expr);
        return any.get().result();
    }


}
