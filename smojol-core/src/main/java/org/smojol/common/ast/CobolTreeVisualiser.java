package org.smojol.common.ast;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import hu.webarticum.treeprinter.printer.listing.ListingTreePrinter;
import org.smojol.common.flowchart.ConsoleColors;
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

    /**
     *
     * @param serialisableAST
     * @param cobolParseTreeOutputPath
     * @param outputTree
     */
    public void writeCobolAST(CobolContextAugmentedTreeNode serialisableAST, String cobolParseTreeOutputPath, boolean outputTree) {
        if (outputTree) new ListingTreePrinter().print(serialisableAST);
        LOGGER.info(ConsoleColors.green(String.format("Memory usage: %s", Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory())));
        Gson gson = new GsonBuilder().excludeFieldsWithoutExposeAnnotation().setPrettyPrinting().create();
        try (JsonWriter writer = new JsonWriter(resourceOperations.fileWriter(cobolParseTreeOutputPath))) {
            writer.setIndent("  ");
            gson.toJson(serialisableAST, CobolContextAugmentedTreeNode.class, writer);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
