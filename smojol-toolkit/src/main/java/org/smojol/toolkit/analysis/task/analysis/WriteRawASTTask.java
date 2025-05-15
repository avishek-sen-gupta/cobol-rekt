package org.smojol.toolkit.analysis.task.analysis;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.BuildSerialisableASTTask;
import org.smojol.common.ast.CobolContextAugmentedTreeNode;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.resource.ResourceOperations;
import com.mojo.algorithms.task.CommandLineAnalysisTask;
import com.mojo.algorithms.task.AnalysisTask;
import com.mojo.algorithms.task.AnalysisTaskResult;
import org.smojol.toolkit.analysis.pipeline.config.RawASTOutputConfig;

import java.io.IOException;
import java.util.logging.Logger;

public class WriteRawASTTask implements AnalysisTask {
    private static final Logger LOGGER = Logger.getLogger(WriteRawASTTask.class.getName());
    private final ParseTree tree;
    private final CobolEntityNavigator navigator;
    private final RawASTOutputConfig rawAstOutputConfig;
    private final ResourceOperations resourceOperations;

    public WriteRawASTTask(CobolEntityNavigator navigator, RawASTOutputConfig rawAstOutputConfig, ResourceOperations resourceOperations) {
        this.tree = navigator.getRoot();
        this.navigator = navigator;
        this.rawAstOutputConfig = rawAstOutputConfig;
        this.resourceOperations = resourceOperations;
    }

    @Override
    public AnalysisTaskResult run() {
        try {
            LOGGER.info(ConsoleColors.green(String.format("Memory usage: %s", Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory())));
            LOGGER.info(String.format("AST Output Dir is: %s", rawAstOutputConfig.astOutputDir()));
            resourceOperations.createDirectories(rawAstOutputConfig.astOutputDir());
//            Files.createDirectories(rawAstOutputConfig.astOutputDir());
            CobolContextAugmentedTreeNode serialisableAST = new BuildSerialisableASTTask().run(tree, navigator);
            rawAstOutputConfig.visualiser().writeCobolAST(serialisableAST, rawAstOutputConfig.cobolParseTreeOutputPath(), false);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.WRITE_RAW_AST, serialisableAST);
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_RAW_AST);
        }
    }
}
