package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;

public class CobolExpressionBuilder {
    public CobolExpression identifier(CobolParser.GeneralIdentifierContext ctx) {
        GeneralIdentifierVisitor identifierVisitor = new GeneralIdentifierVisitor();
        ctx.accept(identifierVisitor);
        return identifierVisitor.getExpression();
    }

    public CobolExpression literal(CobolParser.LiteralContext ctx) {
        LiteralVisitor literalVisitor = new LiteralVisitor();
        ctx.accept(literalVisitor);
        return literalVisitor.getExpression();
    }

    public CobolExpression literalOrIdentifier(CobolParser.LiteralContext literalContext, CobolParser.GeneralIdentifierContext identifierContext) {
        return literalContext != null ? literal(literalContext) : identifier(identifierContext);
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
