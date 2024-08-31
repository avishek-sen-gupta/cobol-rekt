package org.smojol.common.vm.expression;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;

public class PowersVisitor extends AntlrCobolExpressionVisitor {
    @Override
    public CobolExpression visitPowers(CobolParser.PowersContext ctx) {
        BasisVisitor basisVisitor = new BasisVisitor();
        ctx.basis().accept(basisVisitor);
        expression = basisVisitor.getExpression();
        if (ctx.power().isEmpty()) return expression;
        for (CobolParser.PowerContext p : ctx.power()) {
            PowerVisitor powerVisitor = new PowerVisitor();
            p.accept(powerVisitor);
            expression = new ExponentExpression(expression, powerVisitor.getExpression());
        }
        return expression;
    }

    @Override
    public CobolExpression visit(ParseTree parseTree) {
//        if (parseTree.getClass() == CobolParser.BasisContext.class) {
//            BasisVisitor basisVisitor = new BasisVisitor();
//            parseTree.accept(basisVisitor);
//            expression = basisVisitor.getExpression();
//        }
//        else if (parseTree.getClass() == CobolParser.PowerContext.class) {
//            PowerVisitor powerVisitor = new PowerVisitor();
//            parseTree.accept(powerVisitor);
//            expression = new ExponentExpression(expression, powerVisitor.getExpression());
//        }
        return expression;
    }
}
