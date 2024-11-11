package org.smojol.common.ast;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.navigation.CobolEntityNavigator;

import java.io.IOException;
import java.io.StringWriter;
import java.util.logging.Logger;

public class BuildSerialisableASTTask {
    private static final Logger LOGGER = Logger.getLogger(BuildSerialisableASTTask.class.getName());

    /**
     *
     * @param tree
     * @param navigator
     * @return
     */
    public CobolContextAugmentedTreeNode run(ParseTree tree, CobolEntityNavigator navigator) {
        navigator.buildDialectNodeRepository();
        CobolContextAugmentedTreeNode graphRoot = new CobolContextAugmentedTreeNode(tree, navigator);
        buildContextGraph(tree, graphRoot, navigator);
        LOGGER.info(ConsoleColors.green(String.format("Memory usage: %s", Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory())));
        return graphRoot;
    }

    /**
     *
     * @param tree
     * @param navigator
     * @return
     */
    public String asSerialised(ParseTree tree, CobolEntityNavigator navigator) {
        CobolContextAugmentedTreeNode serialisableAST = run(tree, navigator);
        StringWriter stringWriter = new StringWriter();
        Gson gson = new GsonBuilder().excludeFieldsWithoutExposeAnnotation().setPrettyPrinting().create();
        try (JsonWriter writer = new JsonWriter(stringWriter)) {
            writer.setIndent("  ");
            gson.toJson(serialisableAST, CobolContextAugmentedTreeNode.class, writer);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return stringWriter.toString();
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
