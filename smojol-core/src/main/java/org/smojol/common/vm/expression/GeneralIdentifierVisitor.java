package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;

public class GeneralIdentifierVisitor extends CobolExpressionVisitor {
    @Override
    public CobolExpression visitGeneralIdentifier(CobolParser.GeneralIdentifierContext ctx) {
        if (ctx.qualifiedDataName().isEmpty()) return expression;
        expression = new VariableExpression(ctx.qualifiedDataName());
        return expression;
        // TODO: Other forms of general identifiers are not supported yet
    }
}
