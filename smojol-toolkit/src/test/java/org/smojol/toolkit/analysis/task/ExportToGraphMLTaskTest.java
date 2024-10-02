package org.smojol.toolkit.analysis.task;

import org.jgrapht.Graph;
import org.junit.jupiter.api.Test;
import org.smojol.toolkit.analysis.graph.graphml.TypedGraphEdge;
import org.smojol.toolkit.analysis.graph.graphml.TypedGraphVertex;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertTrue;

class ExportToGraphMLTaskTest {
    @Test
    void canCreateDataStructures() throws IOException {
        AnalysisTaskResult taskResult = new TestTaskRunner("no-branches.cbl", "test-code/flow-ast")
                .runTask(CommandLineAnalysisTask.EXPORT_TO_GRAPHML);
        assertTrue(taskResult.isSuccess());
        Graph<TypedGraphVertex, TypedGraphEdge> root = ((AnalysisTaskResultOK) taskResult).getDetail();
    }
}
