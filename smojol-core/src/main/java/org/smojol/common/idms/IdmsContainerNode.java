/*
 * Copyright (c) 2024 Avishek Sen Gupta.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 *
 */

package org.smojol.common.idms;

import org.antlr.v4.runtime.CommonToken;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolLexer;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.flowchart.NodeText;

/**
 * This serves as a container for all IDMS-related code fragments which are then re-inserted
 * back into the parse tree on writing as JSON
 */
public class IdmsContainerNode extends ParserRuleContext {
    private ParseTree dialectNode;

    public IdmsContainerNode(ParseTree dialectNode, CobolParser.DialectNodeFillerContext parent) {
        super(parent, 1);
        this.dialectNode = dialectNode;
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
