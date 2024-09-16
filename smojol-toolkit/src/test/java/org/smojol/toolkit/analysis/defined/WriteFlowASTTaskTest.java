package org.smojol.toolkit.analysis.defined;

import org.junit.jupiter.api.Test;
import org.smojol.common.ast.SerialisableASTFlowNode;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertTrue;

class WriteFlowASTTaskTest {
    @Test
    void canCreateFlowAST() throws IOException {
        AnalysisTaskResult taskResult = new TestTaskRunner("no-branches.cbl", "test-code/flow-ast")
                .runTask(CommandLineAnalysisTask.WRITE_FLOW_AST);
        assertTrue(taskResult.isSuccess());
        SerialisableASTFlowNode root = ((AnalysisTaskResultOK) taskResult).getDetail();
    }
}
