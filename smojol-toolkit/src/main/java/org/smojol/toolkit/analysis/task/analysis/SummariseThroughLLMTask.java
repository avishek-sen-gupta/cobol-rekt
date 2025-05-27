package org.smojol.toolkit.analysis.task.analysis;

import com.mojo.algorithms.navigation.TreeMapperTraversal;
import com.mojo.woof.*;
import org.neo4j.driver.Record;
import com.mojo.algorithms.domain.FlowNodeType;
import org.smojol.toolkit.intermediate.NodeSpecBuilder;
import com.mojo.algorithms.task.AnalysisTask;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.CommandLineAnalysisTask;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static com.mojo.woof.NodeProperties.SECTION_SOURCE;
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
        Record neo4jProgramRoot = sdk.findNodes(qualifier.cfgNodeCriteria(Map.of(TYPE, FlowNodeType.PROCEDURE_DIVISION_BODY.toString()))).getFirst();
        Record neo4jDataStructuresRoot = sdk.findNodes(qualifier.dataNodeSearchCriteria(Map.of(SECTION_SOURCE, "ROOT"))).getFirst();
        Advisor advisor = new AzureOpenAIAdvisor(OpenAICredentials.fromEnv());
        // Summarises AST bottom-up
        Function<Record, List<Record>> codeChildrenFn = n -> sdk.directChildren(n, CONTAINS_CODE);
        Function<Record, List<Record>> dataChildrenFn = n -> sdk.directChildren(n, CONTAINS_DATA);
        new TreeMapperTraversal<Record, ActionResult>().accept(neo4jProgramRoot, new Neo4JCodeSummaryVisitor(advisor, sdk), codeChildrenFn);
        new TreeMapperTraversal<Record, ActionResult>().accept(neo4jDataStructuresRoot, new Neo4JDataSummaryVisitor(advisor, sdk), dataChildrenFn);
    }
}
