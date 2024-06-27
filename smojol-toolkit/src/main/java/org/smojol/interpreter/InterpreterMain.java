package org.smojol.interpreter;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.ast.DisplayFlowNode;
import org.smojol.ast.FlowchartBuilderImpl;
import org.smojol.ast.RunFlowchartTracer;
import org.smojol.analysis.ParsePipeline;
import org.smojol.analysis.visualisation.CobolTreeVisualiserImpl;
import org.smojol.analysis.visualisation.PocOpsImpl;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.interpreter.interpreter.CobolBreakpointer;
import org.smojol.interpreter.interpreter.CobolConditionResolver;
import org.smojol.interpreter.interpreter.CobolInterpreterFactory;
import org.smojol.interpreter.interpreter.RunLogger;
import org.smojol.interpreter.navigation.CobolEntityNavigatorBuilderImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.smojol.common.vm.interpreter.Breakpointer;
import org.smojol.common.vm.interpreter.ExecutionListener;
import org.smojol.common.vm.interpreter.ExecutionListeners;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.strategy.UnresolvedReferenceDoNothingStrategy;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.io.File;
import java.io.IOException;

public class InterpreterMain {
    private final Logger logger = LoggerFactory.getLogger(InterpreterMain.class);

    public static void main(String[] args) throws IOException, InterruptedException {
        File[] copyBookPaths = new File[]{new File("/Users/asgupta/Downloads/mbrdi-poc")};
        String dialectJarPath = "/Users/asgupta/code/mbrdi-proleap/che4z/che-che4z-lsp-for-cobol-2.1.2/server/dialect-idms/target/dialect-idms.jar";
        String cobolParseTreeOutputPath = "/Users/asgupta/Downloads/mbrdi-poc/test-cobol.json";
        String idmsParseTreeOutputPath = "/Users/asgupta/Downloads/mbrdi-poc/test-idms.json";
        String dotFilePath = "/Users/asgupta/Downloads/mbrdi-poc/flowchart.dot";
        String imageOutputPath = "/Users/asgupta/Downloads/mbrdi-poc/flowchart.png";

//        File source = new File("/Users/asgupta/Downloads/mbrdi-poc/V75234");
//        File source = new File("/Users/asgupta/Downloads/mbrdi-poc/V7588049");
//        File source = new File("/Users/asgupta/Downloads/mbrdi-poc/V751C931");
        File source = new File("/Users/asgupta/Downloads/mbrdi-poc/test-exp.cbl");
//        File source = new File("/Users/asgupta/Downloads/mbrdi-poc/occurs-test.cbl");
//        File source = new File("/Users/asgupta/Downloads/mbrdi-poc/test.cbl");
//        File source = new File("/Users/asgupta/Downloads/mbrdi-poc/if-test.cbl");

        PocOpsImpl ops = new PocOpsImpl(new CobolTreeVisualiserImpl(),
                FlowchartBuilderImpl::build, new CobolEntityNavigatorBuilderImpl(), new UnresolvedReferenceDoNothingStrategy());
        ParsePipeline pipeline = new ParsePipeline(source,
                copyBookPaths,
                dialectJarPath,
                cobolParseTreeOutputPath,
                ops);

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
//        bp.addBreakpoint(n -> n.getClass() == IfChartNode.class && n.originalText().contains("SOME-OTHER-CONDITION"));
//        bp.addBreakpoint(n -> n.getClass() == DisplayChartNode.class && n.originalText().contains("Hello, world"));
//        bp.addBreakpoint(n -> n.getClass() == ComputeChartNode.class && n.originalText().contains("WEUR-VALUE(I) ROUNDED"));
//        bp.addBreakpoint(n -> n.getClass() == AddChartNode.class && n.originalText().contains("SOMETEXT"));
        bp.addBreakpoint(n -> n.getClass() == DisplayFlowNode.class && n.originalText().contains("SOMETEXT"));
        ExecutionListener tracer = new RunFlowchartTracer();
        ExecutionListeners executionListeners = new ExecutionListeners(ImmutableList.of(new RunLogger(), tracer));
        root.acceptInterpreter(CobolInterpreterFactory.interpreter(CobolConditionResolver.EVALUATING_RESOLVER, dataStructures, ImmutableList.of(), executionListeners, bp), FlowControl::CONTINUE);
    }
}
