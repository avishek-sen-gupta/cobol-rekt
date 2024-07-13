package org.smojol.analysis.pipeline;

import com.mojo.woof.Advisor;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import com.mojo.woof.OpenAICredentials;
import org.antlr.v4.runtime.tree.ParseTree;
import org.neo4j.driver.Record;
import org.smojol.analysis.ParsePipeline;
import org.smojol.analysis.graph.*;
import org.smojol.analysis.graph.graphml.GraphMLExportCommands;
import org.smojol.analysis.graph.neo4j.*;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;
import org.smojol.common.flowchart.FlowNodeType;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import static com.mojo.woof.NodeProperties.TYPE;
import static com.mojo.woof.NodeRelations.CONTAINS;

public class SmojolPipeline {
    public void run(ParsePipeline pipeline) throws IOException {
        CobolEntityNavigator navigator = pipeline.parse();
        FlowchartBuilder flowcharter = pipeline.flowcharter();
        CobolDataStructure dataStructures = pipeline.getDataStructures();

        ParseTree procedure = navigator.procedureBodyRoot();

        flowcharter.buildFlowAST(procedure).buildControlFlow().buildOverlay();
        FlowNode astRoot = flowcharter.getRoot();
        FlowNodeService nodeService = flowcharter.getChartNodeService();

        NodeSpecBuilder qualifier = new NodeSpecBuilder(new NamespaceQualifier("NEW-CODE"));
        GraphSDK sdk = new GraphSDK(new Neo4JDriverBuilder().fromEnv());
        exportToNeo4J(astRoot, dataStructures, qualifier, sdk);
        exportToGraphML(astRoot, dataStructures, qualifier);
//        summariseThroughLLM(qualifier, sdk);
    }

    private static void exportToGraphML(FlowNode astRoot, CobolDataStructure dataStructures, NodeSpecBuilder qualifier) {
        GraphMLExportCommands graphMLExporter = new GraphMLExportCommands(dataStructures, astRoot, qualifier);
        graphMLExporter.buildAST(new File("/Users/asgupta/code/smojol/out/ast.graphml"));
        graphMLExporter.buildDataStructures(new File("/Users/asgupta/code/smojol/out/data_structures.graphml"));
        graphMLExporter.buildCFG(new File("/Users/asgupta/code/smojol/out/cfg.graphml"));
    }

    private static void exportToNeo4J(FlowNode root, CobolDataStructure dataStructures, NodeSpecBuilder qualifier, GraphSDK sdk) {
        // Builds Control Flow Graph
        root.accept(new Neo4JFlowCFGVisitor(sdk, qualifier), -1);

        // Builds AST
        Neo4JASTExporter neo4JExporter = new Neo4JASTExporter(sdk, dataStructures, qualifier, NodeReferenceStrategy.EXISTING_CFG_NODE, NodeReferenceStrategy.EXISTING_CFG_NODE);
        neo4JExporter.buildAST(root);

        // Builds data structures
        dataStructures.accept(new Neo4JDataStructureVisitor(sdk, qualifier), null, n -> false, dataStructures);
        dataStructures.accept(new Neo4JRedefinitionVisitor(sdk, qualifier), null, n -> false, dataStructures);

        // Builds data dependencies
        neo4JExporter.buildDataDependencies(root);
    }

    private static void summariseThroughLLM(NodeSpecBuilder qualifier, GraphSDK sdk) {
        Record neo4jProgramRoot = sdk.findNodes(qualifier.astNodeCriteria(Map.of(TYPE, FlowNodeType.PROCEDURE_DIVISION_BODY.toString()))).getFirst();
        Record neo4jDataStructuresRoot = sdk.findNodes(qualifier.dataNodeSearchCriteria(Map.of(TYPE, "ROOT"))).getFirst();
        Advisor advisor = new Advisor(OpenAICredentials.fromEnv());
        // Summarises AST bottom-up
        sdk.traverse(neo4jProgramRoot, new SummariseAction(advisor, sdk), CONTAINS);
        // Summarises data structures
        sdk.traverse(neo4jDataStructuresRoot, new DataStructureSummariseAction(advisor, sdk), CONTAINS);
    }
}
