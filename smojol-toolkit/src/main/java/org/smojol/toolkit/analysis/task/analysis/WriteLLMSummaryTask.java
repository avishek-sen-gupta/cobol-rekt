package org.smojol.toolkit.analysis.task.analysis;

import com.mojo.woof.Advisor;
import com.mojo.woof.Neo4JDriverBuilder;
import com.mojo.woof.OpenAICredentials;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.navigation.TreeMapperTraversal;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.util.List;
import java.util.function.Function;

public class WriteLLMSummaryTask implements AnalysisTask {
    public WriteLLMSummaryTask(Neo4JDriverBuilder neo4JDriverBuilder, NodeSpecBuilder qualifier) {
    }

    @Override
    public AnalysisTaskResult run() {
        return AnalysisTaskResult.OK(CommandLineAnalysisTask.SUMMARISE_THROUGH_LLM);
    }


    private static void summariseThroughLLM(FlowNode flowRoot, CobolDataStructure dataRoot) {
        Advisor advisor = new Advisor(OpenAICredentials.fromEnv());
        Function<FlowNode, List<FlowNode>> codeChildrenFn = FlowNode::astChildren;
        Function<CobolDataStructure, List<CobolDataStructure>> dataChildrenFn = CobolDataStructure::subStructures;
        new TreeMapperTraversal<FlowNode, SummaryTree>().accept(flowRoot, new CodeSummaryVisitor(advisor), codeChildrenFn);
        new TreeMapperTraversal<CobolDataStructure, SummaryTree>().accept(dataRoot, new DataSummaryVisitor(advisor), dataChildrenFn);
    }
}
