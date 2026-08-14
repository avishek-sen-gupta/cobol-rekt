package org.smojol.toolkit.task;

import com.mojo.algorithms.id.IdProvider;
import com.mojo.algorithms.task.AnalysisTask;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.CommandLineAnalysisTask;
import java.io.IOException;
import java.util.logging.Logger;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.BuildSerialisableASTTask;
import org.smojol.common.ast.CobolContextAugmentedTreeNode;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.toolkit.analysis.pipeline.config.RawASTOutputConfig;

public class WriteRawASTOnlyTask implements AnalysisTask {
  private static final Logger LOGGER = Logger.getLogger(WriteRawASTOnlyTask.class.getName());
  private final ParsePipeline pipeline;
  private final RawASTOutputConfig rawAstOutputConfig;
  private final ResourceOperations resourceOperations;
  private final IdProvider idProvider;

  public WriteRawASTOnlyTask(
      ParsePipeline pipeline,
      RawASTOutputConfig rawAstOutputConfig,
      ResourceOperations resourceOperations,
      IdProvider idProvider) {
    this.pipeline = pipeline;
    this.rawAstOutputConfig = rawAstOutputConfig;
    this.resourceOperations = resourceOperations;
    this.idProvider = idProvider;
  }

  @Override
  public AnalysisTaskResult run() {
    try {
      LOGGER.info(
          ConsoleColors.green(
              String.format(
                  "Memory usage: %s",
                  Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory())));
      LOGGER.info(String.format("AST Output Dir is: %s", rawAstOutputConfig.astOutputDir()));
      CobolEntityNavigator navigator = pipeline.parse();
      CobolParser.ProcedureDivisionBodyContext rawAST =
          navigator.procedureDivisionBody(navigator.getRoot());
      CobolContextAugmentedTreeNode serialisableAST =
          new BuildSerialisableASTTask().run(rawAST, navigator);
      resourceOperations.createDirectories(rawAstOutputConfig.astOutputDir());
      rawAstOutputConfig
          .visualiser()
          .writeCobolAST(serialisableAST, rawAstOutputConfig.cobolParseTreeOutputPath(), false);
      return AnalysisTaskResult.OK(CommandLineAnalysisTask.WRITE_RAW_AST_ONLY, serialisableAST);
    } catch (IOException e) {
      return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_RAW_AST_ONLY);
    }
  }
}
