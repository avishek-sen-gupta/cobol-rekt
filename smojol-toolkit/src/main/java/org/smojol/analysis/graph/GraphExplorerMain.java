package org.smojol.analysis.graph;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.neo4j.driver.Record;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.smojol.analysis.LanguageDialect;
import org.smojol.analysis.ParsePipeline;
import org.smojol.analysis.visualisation.CobolTreeVisualiserImpl;
import org.smojol.analysis.visualisation.ComponentsBuilder;
import org.smojol.ast.FlowchartBuilderImpl;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;
import org.smojol.common.flowchart.FlowNodeType;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.strategy.UnresolvedReferenceDoNothingStrategy;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.interpreter.navigation.CobolEntityNavigatorBuilderImpl;
import org.smojol.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import static com.mojo.woof.NodeLabels.AST_NODE;
import static com.mojo.woof.NodeLabels.DATA_STRUCTURE;
import static com.mojo.woof.NodeProperties.TYPE;
import static com.mojo.woof.NodeRelations.CONTAINS;

public class GraphExplorerMain {
    private final Logger logger = LoggerFactory.getLogger(GraphExplorerMain.class);

    public static void main(String[] args) throws IOException, InterruptedException {
        File[] copyBookPaths = new File[]{new File("/Users/asgupta/code/smojol/smojol-test-code")};
        String dialectJarPath = "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar";
        String cobolParseTreeOutputPath = "/Users/asgupta/code/smojol/out/test-cobol.json";

        File source = new File("/Users/asgupta/code/smojol/smojol-test-code/test-exp.cbl");

        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiserImpl(),
                FlowchartBuilderImpl::build, new CobolEntityNavigatorBuilderImpl(), new UnresolvedReferenceDoNothingStrategy(),
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
        FlowNode root = flowcharter.getRoot();
        FlowNodeService nodeService = flowcharter.getChartNodeService();

        GraphSDK sdk = new GraphSDK(new Neo4JDriverBuilder().fromEnv());

        // Builds Control Flow Graph
        root.accept(new Neo4JFlowVisitor(sdk), -1);
        Neo4JASTWalker astWalker = new Neo4JASTWalker(sdk, dataStructures);

        // Builds AST
        astWalker.buildAST(root);

        Advisor advisor = new Advisor(OpenAICredentials.fromEnv());
        Record neo4jProgramRoot = sdk.findNode(ImmutableList.of(AST_NODE), Map.of(TYPE, FlowNodeType.PROCEDURE_DIVISION_BODY.toString())).getFirst();

        // Summarises AST bottom-up
        sdk.traverse(neo4jProgramRoot, new SummariseAction(advisor, sdk), CONTAINS);

        // Builds data structures
        dataStructures.accept(new Neo4JDataStructureVisitor(sdk), null, n -> false);

        // Builds data dependencies
        astWalker.buildDataDependencies(root);
        Record neo4jDataStructuresRoot = sdk.findNode(ImmutableList.of(DATA_STRUCTURE, "ROOT"), Map.of()).getFirst();

        // Summarises data structures
        sdk.traverse(neo4jDataStructuresRoot, new DataStructureSummariseAction(advisor, sdk), CONTAINS);
    }
}
