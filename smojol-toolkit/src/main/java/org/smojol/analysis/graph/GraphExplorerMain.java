package org.smojol.analysis.graph;

import com.mojo.woof.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.neo4j.driver.Record;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.smojol.analysis.LanguageDialect;
import org.smojol.analysis.ParsePipeline;
import org.smojol.analysis.visualisation.ComponentsBuilder;
import org.smojol.ast.FlowchartBuilderImpl;
import org.smojol.common.flowchart.*;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.vm.strategy.UnresolvedReferenceDoNothingStrategy;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import static com.mojo.woof.NodeProperties.TYPE;
import static com.mojo.woof.NodeRelations.CONTAINS;

public class GraphExplorerMain {
    private final Logger logger = LoggerFactory.getLogger(GraphExplorerMain.class);

    public static void main(String[] args) throws IOException, InterruptedException {
        File[] copyBookPaths = new File[]{new File("/Users/asgupta/code/smojol/smojol-test-code")};
        String dialectJarPath = "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar";
        String cobolParseTreeOutputPath = "/Users/asgupta/code/smojol/out/test-cobol.json";

        File source = new File("/Users/asgupta/code/smojol/smojol-test-code/test-exp.cbl");

        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiser(),
                FlowchartBuilderImpl::build, new EntityNavigatorBuilder(), new UnresolvedReferenceDoNothingStrategy(),
                new OccursIgnoringFormat1DataStructureBuilder());
        ParsePipeline pipeline = new ParsePipeline(source,
                copyBookPaths,
                dialectJarPath,
                cobolParseTreeOutputPath,
                ops, LanguageDialect.COBOL);

        CobolEntityNavigator navigator = pipeline.parse();
        FlowchartBuilder flowcharter = pipeline.flowcharter();
        CobolDataStructure dataStructures = pipeline.getDataStructures();

        ParseTree procedure = navigator.procedureBodyRoot();

        flowcharter.buildChartAST(procedure).buildControlFlow().buildOverlay();
        FlowNode astRoot = flowcharter.getRoot();
        FlowNodeService nodeService = flowcharter.getChartNodeService();

        NodeSpecBuilder qualifier = new NodeSpecBuilder(new NamespaceQualifier("NEW-CODE"));
        GraphSDK sdk = new GraphSDK(new Neo4JDriverBuilder().fromEnv());
//        exportToNeo4J(root, dataStructures, sdk);

//        GraphPatternMatcher visitor = new GraphPatternMatcher(sdk);
//        root.accept(visitor, node -> node.type() == FlowNodeType.SENTENCE, -1);
//        System.out.printf("Number of groups = %s%n", visitor.getMatches().size());

        GraphMLExportCommands graphMLExporter = new GraphMLExportCommands(dataStructures, astRoot, qualifier);
        graphMLExporter.buildAST();
        graphMLExporter.buildDataStructures();

//        summariseThroughLLM(qualifier, sdk);
    }

    private static void exportToNeo4J(FlowNode root, CobolDataStructure dataStructures, GraphSDK sdk) {
        // Builds Control Flow Graph
        NodeSpecBuilder qualifier = new NodeSpecBuilder(new NamespaceQualifier("NEW-CODE"));
        root.accept(new Neo4JFlowCFGVisitor(sdk, qualifier), -1);
        Neo4JASTExporter neo4JExporter = new Neo4JASTExporter(sdk, dataStructures, qualifier);

        // Builds AST
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
