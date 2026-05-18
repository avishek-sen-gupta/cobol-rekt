package org.smojol.toolkit.analysis.task;

import static org.junit.jupiter.api.Assertions.assertTrue;

import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.AnalysisTaskResultOK;
import com.mojo.algorithms.task.CommandLineAnalysisTask;
import java.io.IOException;
import org.junit.jupiter.api.Test;
import org.smojol.toolkit.analysis.graph.graphml.SerialisableUnifiedModel;

public class ExportUnifiedTaskRegressionTest {
  @Test
  void canCreateDataStructures() throws IOException {
    AnalysisTaskResult taskResult =
        new TestTaskRunner("no-branches.cbl", "test-code/flow-ast")
            .runTask(CommandLineAnalysisTask.EXPORT_UNIFIED_TO_JSON);
    assertTrue(taskResult.isSuccess());
    SerialisableUnifiedModel root = ((AnalysisTaskResultOK) taskResult).getDetail();
  }
}
