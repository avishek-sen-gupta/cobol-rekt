package org.smojol.toolkit.analysis.task;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.smojol.toolkit.analysis.task.TestTaskRunner.dir;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.id.UUIDProvider;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.AnalysisTaskResultOK;
import com.mojo.algorithms.task.CommandLineAnalysisTask;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.smojol.common.ast.CobolContextAugmentedTreeNode;
import org.smojol.common.ast.CobolTreeVisualiser;
import org.smojol.common.dependency.ComponentsBuilder;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.logging.LoggingConfig;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.vm.strategy.UnresolvedReferenceThrowStrategy;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.toolkit.analysis.pipeline.config.RawASTOutputConfig;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.task.WriteRawASTOnlyTask;

class WriteRawASTOnlyTaskRegressionTest {
  private static final String PROGRAM = "no-branches.cbl";
  private static final String SOURCE_DIR = "test-code/flow-ast";

  @Test
  void canCreateRawASTWithoutBuildingBaseModel() throws IOException {
    List<AnalysisTaskResult> results =
        new TestTaskRunner(PROGRAM, SOURCE_DIR)
            .allResults(CommandLineAnalysisTask.WRITE_RAW_AST_ONLY);

    assertEquals(1, results.size());
    AnalysisTaskResult taskResult = results.getFirst();
    assertTrue(taskResult.isSuccess());
    assertEquals(
        CommandLineAnalysisTask.WRITE_RAW_AST_ONLY.name(),
        ((AnalysisTaskResultOK) taskResult).getTask());
    CobolContextAugmentedTreeNode root = ((AnalysisTaskResultOK) taskResult).getDetail();
    assertNotNull(root);
    assertFalse(root.children().isEmpty());
  }

  @Test
  void canCreateRawASTForUnsupportedPictureString() throws IOException {
    List<AnalysisTaskResult> results =
        new TestTaskRunner("floating-currency.cbl", SOURCE_DIR)
            .allResults(CommandLineAnalysisTask.WRITE_RAW_AST_ONLY);

    assertEquals(1, results.size());
    AnalysisTaskResult taskResult = results.getFirst();
    assertTrue(taskResult.isSuccess());
    CobolContextAugmentedTreeNode root = ((AnalysisTaskResultOK) taskResult).getDetail();
    assertNotNull(root);
    assertFalse(root.children().isEmpty());
  }

  @Test
  void doesNotBuildDataStructures() throws IOException {
    ParsePipeline pipeline = pipeline();
    AnalysisTaskResult taskResult =
        new WriteRawASTOnlyTask(
                pipeline, rawASTOutputConfig(), new LocalFilesystemOperations(), new UUIDProvider())
            .run();

    assertTrue(taskResult.isSuccess());
    assertTrue(pipeline.getDataStructures().subStructures().isEmpty());

    ParsePipeline buildingPipeline = pipeline();
    buildingPipeline.parse();
    assertFalse(buildingPipeline.getDataStructures().subStructures().isEmpty());
  }

  private static ParsePipeline pipeline() {
    LoggingConfig.setupLogging();
    LocalFilesystemOperations resourceOperations = new LocalFilesystemOperations();
    SourceConfig sourceConfig =
        new SourceConfig(
            PROGRAM,
            dir(SOURCE_DIR),
            ImmutableList.of(new File(dir(SOURCE_DIR))),
            dir(
                "../che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar"));
    ComponentsBuilder ops =
        new ComponentsBuilder(
            new CobolTreeVisualiser(resourceOperations),
            new EntityNavigatorBuilder(),
            new UnresolvedReferenceThrowStrategy(),
            new OccursIgnoringFormat1DataStructureBuilder(),
            new UUIDProvider(),
            resourceOperations);
    return new ParsePipeline(sourceConfig, ops, LanguageDialect.COBOL);
  }

  private static RawASTOutputConfig rawASTOutputConfig() {
    Path astOutputDir = Paths.get(dir("test-code/out"), PROGRAM + ".report", "ast");
    return new RawASTOutputConfig(
        astOutputDir,
        astOutputDir.resolve(String.format("cobol-%s.json", PROGRAM)).toString(),
        new CobolTreeVisualiser());
  }
}
