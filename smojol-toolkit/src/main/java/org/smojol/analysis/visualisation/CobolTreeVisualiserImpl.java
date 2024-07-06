package org.smojol.analysis.visualisation;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import hu.webarticum.treeprinter.printer.listing.ListingTreePrinter;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.flowchart.CobolContextAugmentedTreeNode;
import org.smojol.common.flowchart.CobolTreeVisualiser;

import java.io.FileNotFoundException;
import java.io.PrintWriter;

/**
 * Draws Cobol AST
 */
public class CobolTreeVisualiserImpl implements CobolTreeVisualiser {
    @Override
    public void writeCobolAST(ParserRuleContext tree, String cobolParseTreeOutputPath, CobolEntityNavigator navigator) {
        writeCobolAST(tree, cobolParseTreeOutputPath, true, navigator);
    }

    /**
     * Draws the tree using the canonical Context structure
     *
     * @param tree
     * @param cobolParseTreeOutputPath
     */
    @Override
    public void writeCobolAST(ParseTree tree, String cobolParseTreeOutputPath, boolean outputTree, CobolEntityNavigator navigator) {
        navigator.buildDialectNodeRepository();
        Gson gson = new GsonBuilder().excludeFieldsWithoutExposeAnnotation().setPrettyPrinting().create();
        CobolContextAugmentedTreeNode graphRoot = new CobolContextAugmentedTreeNode(tree, navigator);
        buildContextGraph(tree, graphRoot, navigator);
        if (outputTree) new ListingTreePrinter().print(graphRoot);
        try {
            String s = gson.toJson(graphRoot);
//            PrintWriter out = new PrintWriter("/Users/asgupta/Downloads/mbrdi-poc/V7523438-compiled-parse-tree.json");
            PrintWriter out = new PrintWriter(cobolParseTreeOutputPath);
            out.println(s);
            out.close();
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    private void buildContextGraph(ParseTree astParentNode, CobolContextAugmentedTreeNode graphParentNode, CobolEntityNavigator navigator) {
        for (int i = 0; i <= astParentNode.getChildCount() - 1; ++i) {
            ParseTree astChildNode = astParentNode.getChild(i);
            CobolContextAugmentedTreeNode graphChildNode = new CobolContextAugmentedTreeNode(astChildNode, navigator);
            graphParentNode.addChild(graphChildNode);
            buildContextGraph(astChildNode, graphChildNode, navigator);
        }
        graphParentNode.freeze();
    }
}
