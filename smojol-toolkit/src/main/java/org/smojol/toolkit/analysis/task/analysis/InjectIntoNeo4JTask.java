package org.smojol.toolkit.analysis.task.analysis;

import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import org.smojol.common.ast.FlowNode;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.neo4j.Neo4JDataStructureVisitor;
import org.smojol.toolkit.analysis.graph.neo4j.Neo4JFlowCFGVisitor;
import org.smojol.toolkit.analysis.graph.neo4j.Neo4JGraphBuilder;
import org.smojol.toolkit.analysis.graph.neo4j.Neo4JRedefinitionVisitor;
import org.smojol.toolkit.analysis.pipeline.config.GraphBuildConfig;

public class InjectIntoNeo4JTask implements AnalysisTask {
    private final FlowNode astRoot;
    private final CobolDataStructure dataStructures;
    private final NodeSpecBuilder qualifier;
    private final Neo4JDriverBuilder neo4JDriverBuilder;
    private final GraphBuildConfig graphBuildConfig;

    public InjectIntoNeo4JTask(FlowNode astRoot, CobolDataStructure dataStructures, NodeSpecBuilder qualifier, Neo4JDriverBuilder neo4JDriverBuilder, GraphBuildConfig graphBuildConfig) {
        this.astRoot = astRoot;
        this.dataStructures = dataStructures;
        this.qualifier = qualifier;
        this.neo4JDriverBuilder = neo4JDriverBuilder;
        this.graphBuildConfig = graphBuildConfig;
    }

    public AnalysisTaskResult run() {
        try (GraphSDK graphSDK = new GraphSDK(neo4JDriverBuilder.fromEnv())) {
            exportToNeo4J(astRoot, dataStructures, qualifier, graphSDK);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.INJECT_INTO_NEO4J);
        } catch (Exception e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.INJECT_INTO_NEO4J);
        }
    }

    private void exportToNeo4J(FlowNode root, CobolDataStructure dataStructures, NodeSpecBuilder qualifier, GraphSDK sdk) {
        // Builds Control Flow Graph
        root.accept(new Neo4JFlowCFGVisitor(sdk, qualifier), -1);

        // Builds AST
        Neo4JGraphBuilder neo4JExporter = new Neo4JGraphBuilder(sdk, dataStructures, qualifier, graphBuildConfig);
        neo4JExporter.buildAST(root);

        // Builds data structures
        dataStructures.accept(new Neo4JDataStructureVisitor(sdk, qualifier), null, n -> false, dataStructures);
        dataStructures.accept(new Neo4JRedefinitionVisitor(sdk, qualifier), null, n -> false, dataStructures);

        // Builds data dependencies
        neo4JExporter.buildDataDependencies(root);
    }
}
