package org.smojol.toolkit.analysis.task.analysis;

import com.mojo.woof.Advisor;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import com.mojo.woof.OpenAICredentials;
import org.neo4j.driver.Record;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.analysis.graph.DataStructureSummariseAction;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.SummariseAction;

import java.util.Map;

import static com.mojo.woof.NodeProperties.TYPE;
import static com.mojo.woof.NodeRelations.CONTAINS_CODE;
import static com.mojo.woof.NodeRelations.CONTAINS_DATA;

public class SummariseThroughLLMTask implements AnalysisTask {
    private final Neo4JDriverBuilder neo4JDriverBuilder;
    private final NodeSpecBuilder qualifier;

    public SummariseThroughLLMTask(Neo4JDriverBuilder neo4JDriverBuilder, NodeSpecBuilder qualifier) {
        this.neo4JDriverBuilder = neo4JDriverBuilder;
        this.qualifier = qualifier;
    }

    @Override
    public AnalysisTaskResult run() {
        try (GraphSDK graphSDK = new GraphSDK(neo4JDriverBuilder.fromEnv())) {
            summariseThroughLLM(qualifier, graphSDK);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.SUMMARISE_THROUGH_LLM);
        } catch (Exception e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.SUMMARISE_THROUGH_LLM);
        }
    }

    private static void summariseThroughLLM(NodeSpecBuilder qualifier, GraphSDK sdk) {
        Record neo4jProgramRoot = sdk.findNodes(qualifier.astNodeCriteria(Map.of(TYPE, FlowNodeType.PROCEDURE_DIVISION_BODY.toString()))).getFirst();
        Record neo4jDataStructuresRoot = sdk.findNodes(qualifier.dataNodeSearchCriteria(Map.of(TYPE, "ROOT"))).getFirst();
        Advisor advisor = new Advisor(OpenAICredentials.fromEnv());
        // Summarises AST bottom-up
        sdk.traverse(neo4jProgramRoot, new SummariseAction(advisor, sdk), CONTAINS_CODE);
        // Summarises data structures
        sdk.traverse(neo4jDataStructuresRoot, new DataStructureSummariseAction(advisor, sdk), CONTAINS_DATA);
    }
}
