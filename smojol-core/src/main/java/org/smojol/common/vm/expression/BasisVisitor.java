package org.smojol.common.vm.expression;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.type.AbstractCobolType;

public class BasisVisitor extends AntlrCobolExpressionVisitor {
    @Override
    public CobolExpression visitBasis(CobolParser.BasisContext ctx) {
        if (ctx.generalIdentifier() != null) {
            expression = new CobolExpressionBuilder().identifier(ctx.generalIdentifier());
        } else if (ctx.arithmeticExpression() != null) {
            expression = new CobolExpressionBuilder().arithmetic(ctx.arithmeticExpression());
        } else if (ctx.literal() != null) {
            expression = new CobolExpressionBuilder().literal(ctx.literal(), AbstractCobolType.NUMBER);
        } else if (ctx.dialectNodeFiller() != null) {
            CobolEntityNavigator navigator = new CobolEntityNavigator(ctx);
            ParseTree entity = navigator.findByCondition(ctx, n -> "Idms_db_entity_nameContext".equals(n.getClass().getSimpleName()));
            expression = new IdmsExpression(entity);
        }

        return expression;
    }
}
