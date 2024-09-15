package org.smojol.toolkit.analysis.defined;

import org.antlr.v4.runtime.ParserRuleContext;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.analysis.pipeline.config.RawASTOutputConfig;

import java.io.IOException;
import java.util.logging.Logger;

public class WriteRawASTTask implements AnalysisTask {
    private static final Logger LOGGER = Logger.getLogger(WriteRawASTTask.class.getName());
    private final ParserRuleContext tree;
    private final CobolEntityNavigator navigator;
    private final RawASTOutputConfig rawAstOutputConfig;
    private final ResourceOperations resourceOperations;

    public WriteRawASTTask(CobolEntityNavigator navigator, RawASTOutputConfig rawAstOutputConfig, ResourceOperations resourceOperations) {
        this.tree = navigator.getFullProgramTree();
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
            rawAstOutputConfig.visualiser().writeCobolAST(tree, rawAstOutputConfig.cobolParseTreeOutputPath(), false, navigator);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.WRITE_RAW_AST);
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_RAW_AST);
        }
    }
}
