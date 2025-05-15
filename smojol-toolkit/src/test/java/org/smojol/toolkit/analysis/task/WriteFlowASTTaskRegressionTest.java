package org.smojol.toolkit.analysis.task;

import org.junit.jupiter.api.Test;
import org.smojol.common.ast.SerialisableASTFlowNode;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.AnalysisTaskResultOK;
import com.mojo.algorithms.task.CommandLineAnalysisTask;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertTrue;

class WriteFlowASTTaskRegressionTest {
    @Test
    void canCreateFlowAST() throws IOException {
        AnalysisTaskResult taskResult = new TestTaskRunner("no-branches.cbl", "test-code/flow-ast")
                .runTask(CommandLineAnalysisTask.WRITE_FLOW_AST);
        assertTrue(taskResult.isSuccess());
        SerialisableASTFlowNode root = ((AnalysisTaskResultOK) taskResult).getDetail();
    }
}
