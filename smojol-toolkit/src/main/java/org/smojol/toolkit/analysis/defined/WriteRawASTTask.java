package org.smojol.toolkit.analysis.defined;

import org.antlr.v4.runtime.ParserRuleContext;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.analysis.pipeline.config.RawASTOutputConfig;

import java.io.IOException;
import java.nio.file.Files;

public class WriteRawASTTask implements AnalysisTask {
    private final ParserRuleContext tree;
    private final CobolEntityNavigator navigator;
    private final RawASTOutputConfig rawAstOutputConfig;

    public WriteRawASTTask(ParserRuleContext tree, CobolEntityNavigator navigator, RawASTOutputConfig rawAstOutputConfig) {
        this.tree = tree;
        this.navigator = navigator;
        this.rawAstOutputConfig = rawAstOutputConfig;
    }

    @Override
    public AnalysisTaskResult run() {
        try {
            System.out.println(ConsoleColors.green(String.format("Memory usage: %s", Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory())));
            System.out.printf("AST Output Dir is: %s%n", rawAstOutputConfig.astOutputDir());
            Files.createDirectories(rawAstOutputConfig.astOutputDir());
            rawAstOutputConfig.visualiser().writeCobolAST(tree, rawAstOutputConfig.cobolParseTreeOutputPath(), false, navigator);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.WRITE_RAW_AST);
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_RAW_AST);
        }
    }
}
