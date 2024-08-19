package org.smojol.toolkit.ast;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.core.CobolParserBaseListener;

public class FlowchartNodeListener extends CobolParserBaseListener {
    @Override
    public void enterStatement(CobolParser.StatementContext ctx) {
        super.enterStatement(ctx);
    }

    @Override
    public void enterIfThen(CobolParser.IfThenContext ctx)
    {
        super.enterIfThen(ctx);
    }

    @Override
    public void enterIfStatement(CobolParser.IfStatementContext ctx) {
        System.out.println("ENTERED AN IF STATEMENT");
    }

    @Override public void enterParagraph(CobolParser.ParagraphContext ctx) {
        System.out.println(ctx.paragraphDefinitionName());
    }
}
