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

import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.common.poc.PersistentData;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.core.CobolParserBaseListener;

/**
 * This is a visitor into the generated parse tree which re-integrates the IDMS code fragments
 * which were removed at the time of parsing standard Cobol
 */
public class DialectIntegratorListener extends CobolParserBaseListener {
    @Getter
    private int restores = 0;

    @Override
    public void enterDialectNodeFiller(CobolParser.DialectNodeFillerContext ctx) {
        if (ctx.dialectGuid() == null)
            return;
        super.enterDialectNodeFiller(ctx);
        String guid = ctx.dialectGuid().getText();
        ParseTree dialectNode = PersistentData.getDialectNode("IDMS-" + guid);
        System.out.println(String.format("Restoring _DIALECT_ %s: %s", guid, dialectNode.getText()));
        ctx.addChild(new IdmsContainerNode(dialectNode, ctx));
        restores++;
    }
}
