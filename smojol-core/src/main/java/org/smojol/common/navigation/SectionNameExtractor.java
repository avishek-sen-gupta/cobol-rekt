package org.smojol.common.navigation;

import org.eclipse.lsp.cobol.core.CobolParser;

public class SectionNameExtractor {
    public String sectionName(CobolParser.SectionOrParagraphContext ctx) {
        return ctx.cobolWord() != null ? ctx.cobolWord().getText() : ctx.integerLiteral(0).getText();
    }
}
