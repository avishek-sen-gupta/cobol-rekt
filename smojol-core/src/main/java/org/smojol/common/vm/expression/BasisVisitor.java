package org.smojol.common.vm.expression;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.navigation.CobolEntityNavigator;

public class BasisVisitor extends CobolExpressionVisitor {
    @Override
    public CobolExpression visitBasis(CobolParser.BasisContext ctx) {
        if (ctx.generalIdentifier() != null) {
            GeneralIdentifierVisitor identifierVisitor = new GeneralIdentifierVisitor();
            ctx.generalIdentifier().accept(identifierVisitor);
            expression = identifierVisitor.getExpression();
        } else if (ctx.arithmeticExpression() != null) {
            ArithmeticExpressionVisitor arithmeticExpressionVisitor = new ArithmeticExpressionVisitor();
            ctx.arithmeticExpression().accept(arithmeticExpressionVisitor);
            expression = arithmeticExpressionVisitor.getExpression();
        } else if (ctx.literal() != null) {
            LiteralVisitor literalVisitor = new LiteralVisitor();
            ctx.literal().accept(literalVisitor);
            expression = literalVisitor.getExpression();
        } else if (ctx.dialectNodeFiller() != null) {
            CobolEntityNavigator navigator = new CobolEntityNavigator(ctx);
            ParseTree entity = navigator.findByCondition(ctx, n -> "Idms_db_entity_nameContext".equals(n.getClass().getSimpleName()));
            expression = new IdmsExpression(entity);
        }

        return expression;
    }
}
