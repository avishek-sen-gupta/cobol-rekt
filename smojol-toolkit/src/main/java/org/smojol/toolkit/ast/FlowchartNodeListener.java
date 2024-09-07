package org.smojol.toolkit.ast;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.core.CobolParserBaseListener;
import org.smojol.toolkit.analysis.defined.ProgramDependenciesTask;

import java.util.logging.Logger;

public class FlowchartNodeListener extends CobolParserBaseListener {
    java.util.logging.Logger LOGGER = Logger.getLogger(FlowchartNodeListener.class.getName());
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
        LOGGER.info("ENTERED AN IF STATEMENT");
    }

    @Override public void enterParagraph(CobolParser.ParagraphContext ctx) {
        LOGGER.info(ctx.paragraphDefinitionName().getText());
    }
}
