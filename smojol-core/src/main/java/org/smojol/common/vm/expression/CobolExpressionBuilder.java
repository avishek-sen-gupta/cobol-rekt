package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.CobolDataType;
import org.smojol.common.vm.type.LiteralResolver;

public class CobolExpressionBuilder {
    public CobolExpression identifier(CobolParser.GeneralIdentifierContext ctx) {
        GeneralIdentifierVisitor identifierVisitor = new GeneralIdentifierVisitor();
        ctx.accept(identifierVisitor);
        return identifierVisitor.getExpression();
    }

    public CobolExpression literal(CobolParser.LiteralContext ctx) {
        return literal(ctx, CobolDataType.STRING);
    }

    public CobolExpression literalOrIdentifier(CobolParser.LiteralContext literalContext, CobolParser.GeneralIdentifierContext identifierContext) {
        return literalContext != null ? literal(literalContext, CobolDataType.NUMBER) : identifier(identifierContext);
    }

    private CobolExpression literal(CobolParser.LiteralContext ctx, CobolDataType expectedType) {
        return new LiteralResolver().literal(ctx, expectedType);
    }

    public CobolExpression arithmetic(CobolParser.ArithmeticExpressionContext ctx) {
        ArithmeticExpressionVisitor arithmeticExpressionVisitor = new ArithmeticExpressionVisitor();
        ctx.accept(arithmeticExpressionVisitor);
        return arithmeticExpressionVisitor.getExpression();
    }

    public CobolExpression condition(CobolParser.ConditionContext condition, CobolDataStructure dataStructures) {
        ConditionVisitor visitor = new ConditionVisitor(dataStructures);
        condition.accept(visitor);
        return visitor.getExpression();
    }
}
