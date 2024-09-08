package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.FlowNodeSymbolExtractorVisitor;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.pseudocode.SymbolReferenceBuilder;
import org.smojol.toolkit.analysis.graph.NamespaceQualifier;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.pipeline.config.RawASTOutputConfig;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
import org.smojol.common.ast.CobolTreeVisualiser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.toolkit.ast.DisplayFlowNode;
import org.smojol.toolkit.ast.FlowchartBuilderImpl;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.common.dependency.ComponentsBuilder;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.vm.interpreter.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.smojol.common.vm.strategy.UnresolvedReferenceDoNothingStrategy;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.interpreter.navigation.FlowNodeASTTraversal;
import org.smojol.toolkit.interpreter.structure.DefaultFormat1DataStructureBuilder;
import org.smojol.toolkit.interpreter.interpreter.*;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;

public class InterpreterMain {
    private final Logger logger = LoggerFactory.getLogger(InterpreterMain.class);

    public static void main(String[] args) throws IOException {
        SourceConfig testSourceConfig = new SourceConfig(
                "test-exp.cbl", "/Users/asgupta/code/smojol/smojol-test-code",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar");
        RawASTOutputConfig rawASTOutputConfig = new RawASTOutputConfig(Paths.get("/Users/asgupta/code/smojol/out"), "/Users/asgupta/code/smojol/out/test-cobol.json", new CobolTreeVisualiser());
//        File source = new File("/Users/asgupta/code/smojol/smojol-test-code/table-indexing.cbl");
//        File source = new File("/Users/asgupta/code/smojol/smojol-test-code/table-redef.cbl");
//        File source = new File("/Users/asgupta/code/smojol/smojol-test-code/simple-redef.cbl");

        SourceConfig awsCardDemoConfig = new SourceConfig(
                "CBACT01C", "/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cbl/CBACT01C.cbl",
                ImmutableList.of(new File("/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar");

        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiser(),
                FlowchartBuilderImpl::build, new EntityNavigatorBuilder(), new UnresolvedReferenceDoNothingStrategy(),
                new DefaultFormat1DataStructureBuilder(), new UUIDProvider());
        ParsePipeline pipeline = new ParsePipeline(testSourceConfig, ops, LanguageDialect.COBOL);

        CobolEntityNavigator navigator = pipeline.parse();
        FlowchartBuilder flowcharter = pipeline.flowcharter();
        CobolDataStructure dataStructures = pipeline.getDataStructures();

        // This one is root
        ParseTree procedure = navigator.procedureBodyRoot();

        flowcharter.buildFlowAST(procedure).buildControlFlow().buildOverlay();
        FlowNode root = flowcharter.getRoot();
        FlowNodeService nodeService = flowcharter.getChartNodeService();
        new FlowNodeASTTraversal<FlowNode>().accept(root, new FlowNodeSymbolExtractorVisitor(root, new SmojolSymbolTable(dataStructures, new SymbolReferenceBuilder(new UUIDProvider())), dataStructures));

        System.out.println("DATA STRUCTURES\n--------------------------------\n");
        dataStructures.report();
        System.out.println("INTERPRETING\n--------------------------------\n");
        Breakpointer bp = new CobolBreakpointer();
//        bp.addBreakpoint(n -> n.getClass() == DisplayFlowNode.class);
//        bp.addBreakpoint(n -> n.getClass() == AddChartNode.class && n.originalText().contains("SOMETEXT"));
        bp.addBreakpoint(n -> n.getClass() == DisplayFlowNode.class && n.originalText().contains("SOMETEXT"));
        GraphSDK sdk = new GraphSDK(new Neo4JDriverBuilder().fromEnv());
        Neo4JExecutionTracer executionTracer = new Neo4JExecutionTracer(sdk, new NodeSpecBuilder(new NamespaceQualifier("GLOBAL")));
        ExecutionListeners executionListeners = new ExecutionListeners(ImmutableList.of(new RunLogger("/Users/asgupta/code/smojol/out/report.md"), executionTracer));
//        ExecutionListeners executionListeners = new ExecutionListeners(ImmutableList.of(new RunLogger()));
        root.acceptInterpreter(CobolInterpreterFactory.executingInterpreter(CobolConditionResolver.ALWAYS_YES, dataStructures, ImmutableList.of(), executionListeners, bp), FlowControl::CONTINUE);
    }
}
