package org.smojol.toolkit.analysis.task;

import org.junit.jupiter.api.Test;
import org.smojol.toolkit.analysis.pipeline.SerialisableCobolDataStructure;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertTrue;

class WriteDataStructuresTaskRegressionTest {
    @Test
    void canCreateDataStructures() throws IOException {
        AnalysisTaskResult taskResult = new TestTaskRunner("no-branches.cbl", "test-code/flow-ast")
                .runTask(CommandLineAnalysisTask.WRITE_DATA_STRUCTURES);
        assertTrue(taskResult.isSuccess());
        SerialisableCobolDataStructure root = ((AnalysisTaskResultOK) taskResult).getDetail();
    }
}
