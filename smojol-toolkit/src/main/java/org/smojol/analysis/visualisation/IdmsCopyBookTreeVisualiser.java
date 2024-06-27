/*
 * Copyright (c) 2022 Broadcom.
 * The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *    Broadcom, Inc. - initial API and implementation
 *
 */
package org.smojol.analysis.visualisation;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import hu.webarticum.treeprinter.printer.listing.ListingTreePrinter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.dialects.idms.IdmsCopyParser;
import org.smojol.common.flowchart.CobolContextAugmentedTreeNode;

import java.io.FileNotFoundException;
import java.io.PrintWriter;

/**
 * Draws IDMS AST
 */
public class IdmsCopyBookTreeVisualiser {
    /**
     * Draws the tree
     *
     * @param startRuleContext
     * @param idmsParseTreeOutputPath
     */
    public void visualiseIdmsAST(IdmsCopyParser.StartRuleContext startRuleContext, String idmsParseTreeOutputPath) {
        Gson gson = new GsonBuilder().excludeFieldsWithoutExposeAnnotation().setPrettyPrinting().create();

        CobolContextAugmentedTreeNode graphRoot = drawParseTrees(startRuleContext);

        try {
            String s = gson.toJson(graphRoot);
            PrintWriter out = new PrintWriter(idmsParseTreeOutputPath);
            out.println(s);
            out.close();
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    // TODO: Process OCCURS DEPENDING ON
    private CobolContextAugmentedTreeNode drawParseTrees(ParseTree parseRuleContext) {
        CobolContextAugmentedTreeNode graphRoot = new CobolContextAugmentedTreeNode(parseRuleContext, null);
        buildGraph(parseRuleContext, graphRoot);
        new ListingTreePrinter().print(graphRoot);
        return graphRoot;
    }

    // TODO: Process OCCURS DEPENDING ON
    private void buildGraph(ParseTree astParentNode, CobolContextAugmentedTreeNode graphParentNode) {
        for (int i = 0; i <= astParentNode.getChildCount() - 1; ++i) {
            ParseTree astChildNode = astParentNode.getChild(i);
            CobolContextAugmentedTreeNode graphChildNode = new CobolContextAugmentedTreeNode(astChildNode, null);
            graphParentNode.addChild(graphChildNode);
            buildGraph(astChildNode, graphChildNode);
        }
        graphParentNode.freeze();
    }
}
