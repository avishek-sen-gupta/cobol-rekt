package org.smojol.toolkit.analysis.task;

import static org.junit.jupiter.api.Assertions.assertTrue;

import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.AnalysisTaskResultOK;
import com.mojo.algorithms.task.CommandLineAnalysisTask;
import java.io.IOException;
import org.junit.jupiter.api.Test;
import org.smojol.toolkit.analysis.pipeline.SerialisableCobolDataStructure;

class WriteDataStructuresTaskRegressionTest {
  @Test
  void canCreateDataStructures() throws IOException {
    AnalysisTaskResult taskResult =
        new TestTaskRunner("no-branches.cbl", "test-code/flow-ast")
            .runTask(CommandLineAnalysisTask.WRITE_DATA_STRUCTURES);
    assertTrue(taskResult.isSuccess());
    SerialisableCobolDataStructure root = ((AnalysisTaskResultOK) taskResult).getDetail();
  }
}
