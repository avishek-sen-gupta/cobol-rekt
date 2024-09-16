package org.smojol.toolkit.analysis.defined;

import org.junit.jupiter.api.Test;
import org.smojol.toolkit.analysis.graph.graphml.SerialisableUnifiedModel;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertTrue;

class ExportUnifiedTaskTest {
    @Test
    void canCreateDataStructures() throws IOException {
        AnalysisTaskResult taskResult = new TestTaskRunner("no-branches.cbl", "test-code/flow-ast")
                .runTask(CommandLineAnalysisTask.EXPORT_UNIFIED_TO_JSON);
        assertTrue(taskResult.isSuccess());
        SerialisableUnifiedModel root = ((AnalysisTaskResultOK) taskResult).getDetail();
    }
}
