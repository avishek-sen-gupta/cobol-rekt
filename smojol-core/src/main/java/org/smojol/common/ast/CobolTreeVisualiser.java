package org.smojol.common.ast;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import hu.webarticum.treeprinter.printer.listing.ListingTreePrinter;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.resource.ResourceOperations;

import java.io.IOException;
import java.util.logging.Logger;

public class CobolTreeVisualiser {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(CobolTreeVisualiser.class.getName());
    private final ResourceOperations resourceOperations;

    public CobolTreeVisualiser(ResourceOperations resourceOperations) {
        this.resourceOperations = resourceOperations;
    }

    public CobolTreeVisualiser() {
        this(new LocalFilesystemOperations());
    }

    public void writeCobolAST(ParserRuleContext tree, String cobolParseTreeOutputPath, CobolEntityNavigator navigator) {
        writeCobolAST(tree, cobolParseTreeOutputPath, true, navigator);
    }

    /**
     * Draws the tree using the canonical Context structure
     *
     * @param tree
     * @param cobolParseTreeOutputPath
     */
    public void writeCobolAST(ParseTree tree, String cobolParseTreeOutputPath, boolean outputTree, CobolEntityNavigator navigator) {
        navigator.buildDialectNodeRepository();
        Gson gson = new GsonBuilder().excludeFieldsWithoutExposeAnnotation().setPrettyPrinting().create();
        CobolContextAugmentedTreeNode graphRoot = new CobolContextAugmentedTreeNode(tree, navigator);
        buildContextGraph(tree, graphRoot, navigator);
        if (outputTree) new ListingTreePrinter().print(graphRoot);
        LOGGER.info(ConsoleColors.green(String.format("Memory usage: %s", Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory())));
//        try (JsonWriter writer = new JsonWriter(new FileWriter(cobolParseTreeOutputPath))) {
        try (JsonWriter writer = new JsonWriter(resourceOperations.fileWriter(cobolParseTreeOutputPath))) {
            writer.setIndent("  ");
            gson.toJson(graphRoot, CobolContextAugmentedTreeNode.class, writer);
        } catch (IOException e) {
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
