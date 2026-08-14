package org.smojol.toolkit.analysis.task;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.AnalysisTaskResultOK;
import com.mojo.algorithms.task.CommandLineAnalysisTask;
import java.io.IOException;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.smojol.common.ast.CobolContextAugmentedTreeNode;

class WriteRawASTOnlyTaskRegressionTest {
  @Test
  void canCreateRawASTWithoutBuildingBaseModel() throws IOException {
    List<AnalysisTaskResult> results =
        new TestTaskRunner("no-branches.cbl", "test-code/flow-ast")
            .allResults(CommandLineAnalysisTask.WRITE_RAW_AST_ONLY);

    assertEquals(1, results.size());
    AnalysisTaskResult taskResult = results.getFirst();
    assertTrue(taskResult.isSuccess());
    assertEquals(
        CommandLineAnalysisTask.WRITE_RAW_AST_ONLY.name(),
        ((AnalysisTaskResultOK) taskResult).getTask());
    CobolContextAugmentedTreeNode root = ((AnalysisTaskResultOK) taskResult).getDetail();
    assertNotNull(root);
    assertTrue(!root.children().isEmpty());
  }
}
