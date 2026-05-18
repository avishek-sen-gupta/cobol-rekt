package org.smojol.toolkit.analysis.task;

import static org.junit.jupiter.api.Assertions.assertTrue;

import com.mojo.algorithms.domain.TranspilerFlowgraph;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.AnalysisTaskResultOK;
import com.mojo.algorithms.task.CommandLineAnalysisTask;
import java.io.IOException;
import org.junit.jupiter.api.Test;

public class BuildTranspilerFlowgraphTaskRegressionTest {
  @Test
  void canCreateTranspilerTree() throws IOException {
    AnalysisTaskResult taskResult =
        new TestTaskRunner("no-branches.cbl", "test-code/flow-ast")
            .runTask(CommandLineAnalysisTask.BUILD_TRANSPILER_FLOWGRAPH);
    assertTrue(taskResult.isSuccess());
    TranspilerFlowgraph transpilerFlowgraph = ((AnalysisTaskResultOK) taskResult).getDetail();
  }
}
