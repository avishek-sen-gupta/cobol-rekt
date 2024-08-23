package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;

public class GeneralIdentifierVisitor extends CobolExpressionVisitor {
    @Override
    public CobolExpression visitGeneralIdentifier(CobolParser.GeneralIdentifierContext ctx) {
        if (ctx.qualifiedDataName() != null)
            expression = new VariableExpression(ctx.qualifiedDataName());
        else if (ctx.functionCall() != null)
            expression = new FunctionCallExpression(ctx.functionCall());
        else expression = new SpecialRegisterExpression(ctx.specialRegister());
        return expression;
        // TODO: Other forms of general identifiers are not supported yet
    }
}
