package org.smojol.common.idms;

import lombok.Getter;
import org.antlr.v4.runtime.CommonToken;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolLexer;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.NodeText;
import org.eclipse.lsp.cobol.common.poc.LocalisedDialect;

/**
 * This serves as a container for all IDMS-related code fragments which are then re-inserted
 * back into the parse tree on writing as JSON
 */
public class DialectContainerNode extends ParserRuleContext {
    private final ParseTree dialectNode;
    @Getter private final LocalisedDialect dialect;

    public DialectContainerNode(ParseTree dialectNode, CobolParser.DialectNodeFillerContext parent, LocalisedDialect dialect) {
        super(parent, 1);
        this.dialectNode = dialectNode;
        this.dialect = dialect;
        addAnyChild(dialectNode);
    }

    @Override
    public int getChildCount() {
        return 1;
    }

    @Override
    public ParseTree getChild(int i) {
        return dialectNode;
    }

    @Override
    public String getText() {
        return NodeText.originalText(dialectNode, NodeText::PASSTHROUGH);
    }

    @Override
    public Token getStart() {
        return new CommonToken(CobolLexer.COMPUTATIONAL, getText());
    }

    @Override
    public Token getStop() {
        return new CommonToken(CobolLexer.COMPUTATIONAL, getText());
    }
}
