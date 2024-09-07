package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;

public class GeneralIdentifierVisitor extends AntlrCobolExpressionVisitor {
    @Override
    public CobolExpression visitGeneralIdentifier(CobolParser.GeneralIdentifierContext ctx) {
        VariableExpression variableExpression = new VariableExpression(ctx.qualifiedDataName().variableUsageName());
        if (ctx.qualifiedDataName() != null) {
            if (ctx.qualifiedDataName().tableCall() != null)
                expression = new TableCallExpression(variableExpression, ctx.qualifiedDataName().tableCall().arithmeticExpression());
            else expression = variableExpression;
        } else if (ctx.functionCall() != null)
            expression = new FunctionCallExpression(ctx.functionCall());
        else expression = new SpecialRegisterExpression(ctx.specialRegister());
        return expression;
        // TODO: Other forms of general identifiers are not supported yet
    }
}
