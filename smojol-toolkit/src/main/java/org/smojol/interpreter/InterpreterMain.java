package org.smojol.interpreter;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.analysis.LanguageDialect;
import org.smojol.analysis.graph.NamespaceQualifier;
import org.smojol.analysis.graph.NodeSpecBuilder;
import org.smojol.common.flowchart.CobolTreeVisualiser;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.ast.DisplayFlowNode;
import org.smojol.ast.FlowchartBuilderImpl;
import org.smojol.analysis.ParsePipeline;
import org.smojol.analysis.visualisation.ComponentsBuilder;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.vm.interpreter.*;
import org.smojol.interpreter.interpreter.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.smojol.common.vm.strategy.UnresolvedReferenceDoNothingStrategy;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.interpreter.structure.DefaultFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;

import static org.smojol.common.vm.structure.CobolOperations.NO_OP;

public class InterpreterMain {
    private final Logger logger = LoggerFactory.getLogger(InterpreterMain.class);

    public static void main(String[] args) throws IOException, InterruptedException {
        File[] copyBookPaths = new File[]{new File("/Users/asgupta/code/smojol/smojol-test-code")};
        String dialectJarPath = "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar";
        String cobolParseTreeOutputPath = "/Users/asgupta/code/smojol/out/test-cobol.json";

        File source = new File("/Users/asgupta/code/smojol/smojol-test-code/test-exp.cbl");
//        File source = new File("/Users/asgupta/code/smojol/smojol-test-code/table-indexing.cbl");
//        File source = new File("/Users/asgupta/code/smojol/smojol-test-code/table-redef.cbl");
//        File source = new File("/Users/asgupta/code/smojol/smojol-test-code/simple-redef.cbl");

        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiser(),
                FlowchartBuilderImpl::build, new EntityNavigatorBuilder(), new UnresolvedReferenceDoNothingStrategy(),
                new DefaultFormat1DataStructureBuilder());
        ParsePipeline pipeline = new ParsePipeline(source,
                copyBookPaths,
                dialectJarPath,
                cobolParseTreeOutputPath,
                ops, LanguageDialect.COBOL);

        CobolEntityNavigator navigator = pipeline.parse();
        FlowchartBuilder flowcharter = pipeline.flowcharter();
        CobolDataStructure dataStructures = pipeline.getDataStructures();

        // This one is root
        ParseTree procedure = navigator.procedureBodyRoot();

        flowcharter.buildChartAST(procedure).buildControlFlow().buildOverlay();
        FlowNode root = flowcharter.getRoot();
        FlowNodeService nodeService = flowcharter.getChartNodeService();

        System.out.println("DATA STRUCTURES\n--------------------------------\n");
        dataStructures.report();
        System.out.println("INTERPRETING\n--------------------------------\n");
        Breakpointer bp = new CobolBreakpointer();
        bp.addBreakpoint(n -> n.getClass() == DisplayFlowNode.class);
//        bp.addBreakpoint(n -> n.getClass() == AddChartNode.class && n.originalText().contains("SOMETEXT"));
        bp.addBreakpoint(n -> n.getClass() == DisplayFlowNode.class && n.originalText().contains("SOMETEXT"));
        GraphSDK sdk = new GraphSDK(new Neo4JDriverBuilder().fromEnv());
        Neo4JExecutionTracer executionTracer = new Neo4JExecutionTracer(sdk, new NodeSpecBuilder(new NamespaceQualifier("GLOBAL")));
        ExecutionListeners executionListeners = new ExecutionListeners(ImmutableList.of(new RunLogger(), executionTracer));
//        ExecutionListeners executionListeners = new ExecutionListeners(ImmutableList.of(new RunLogger()));
        root.acceptInterpreter(CobolInterpreterFactory.executingInterpreter(CobolConditionResolver.EVALUATING_RESOLVER, dataStructures, ImmutableList.of(), executionListeners, bp), FlowControl::CONTINUE);
    }
}
