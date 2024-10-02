package org.smojol.toolkit.analysis.task;

import org.junit.jupiter.api.Test;
import org.smojol.common.transpiler.TranspilerFlowgraph;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertTrue;

class BuildTranspilerFlowgraphTaskTest {
    @Test
    void canCreateTranspilerTree() throws IOException {
        AnalysisTaskResult taskResult = new TestTaskRunner("no-branches.cbl", "test-code/flow-ast")
                .runTask(CommandLineAnalysisTask.BUILD_TRANSPILER_FLOWGRAPH);
        assertTrue(taskResult.isSuccess());
        TranspilerFlowgraph transpilerFlowgraph = ((AnalysisTaskResultOK) taskResult).getDetail();
    }
}
