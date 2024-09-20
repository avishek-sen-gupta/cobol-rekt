package org.smojol.toolkit.analysis.defined;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import org.antlr.v4.runtime.ParserRuleContext;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.TranspilerInstructionGeneratorVisitor;
import org.smojol.common.flowchart.MermaidGraph;
import org.smojol.common.navigation.AggregatingTranspilerNodeTraversal;
import org.smojol.common.pseudocode.*;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.common.transpiler.*;
import org.smojol.common.typeadapter.RuntimeTypeAdapterFactory;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.pipeline.config.OutputArtifactConfig;
import org.smojol.toolkit.intermediate.IntermediateASTNodeBuilder;
import org.smojol.toolkit.task.*;
import org.smojol.toolkit.transpiler.TranspilerLoopUpdate;
import org.smojol.toolkit.transpiler.TranspilerTreeBuilder;

import java.io.IOException;
import java.util.List;

public class BuildTranspilerModelTask implements AnalysisTask {
    private final ParserRuleContext rawAST;
    private final CobolDataStructure dataStructures;
    private final SmojolSymbolTable symbolTable;
    private final OutputArtifactConfig transpilerModelOutputConfig;
    private final ResourceOperations resourceOperations;

    public BuildTranspilerModelTask(ParserRuleContext rawAST, CobolDataStructure dataStructures, SmojolSymbolTable symbolTable, OutputArtifactConfig transpilerModelOutputConfig, ResourceOperations resourceOperations) {
        this.rawAST = rawAST;
        this.dataStructures = dataStructures;
        this.symbolTable = symbolTable;
        this.transpilerModelOutputConfig = transpilerModelOutputConfig;
        this.resourceOperations = resourceOperations;
    }

    @Override
    public AnalysisTaskResult run() {
        FlowNode flowRoot = new IntermediateASTNodeBuilder(rawAST, dataStructures, symbolTable).build();
        TranspilerNode transpilerTree = TranspilerTreeBuilder.flowToTranspiler(flowRoot, dataStructures);
        TranspilerInstructionGeneratorVisitor visitor = new TranspilerInstructionGeneratorVisitor(new IncrementingIdProvider());
        new AggregatingTranspilerNodeTraversal<List<TranspilerInstruction>>().accept(transpilerTree, visitor);
        List<TranspilerInstruction> instructions = visitor.result();
        TranspilerModel model = new TranspilerModelBuilder(instructions, transpilerTree).build();
//        System.out.println(instructions);
        Graph<TranspilerInstruction, DefaultEdge> jgraph = new DefaultDirectedGraph<>(DefaultEdge.class);
        instructions.forEach(jgraph::addVertex);
        model.instructionEdges().forEach(edge -> jgraph.addEdge(edge.from(), edge.to()));
        model.pruneUnreachables(jgraph);
        MermaidGraph<TranspilerInstruction, DefaultEdge> mermaid = new MermaidGraph<>();

        try {
            resourceOperations.createDirectories(transpilerModelOutputConfig.outputDir());
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.BUILD_TRANSPILER_MODEL);
        }

        try (JsonWriter writer = new JsonWriter(resourceOperations.fileWriter(transpilerModelOutputConfig.fullPath()))) {
            Gson gson = initGson();
            writer.setIndent("  ");
            gson.toJson(model, TranspilerModel.class, writer);
            String draw = mermaid.draw(jgraph);
            List<String> reductions = new FlowgraphTransformer<>(jgraph, (a, b) -> new DefaultEdge()).reduce();
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.BUILD_TRANSPILER_MODEL, model);
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.BUILD_TRANSPILER_MODEL);
        }
    }

    private static Gson initGson() {
        RuntimeTypeAdapterFactory<TranspilerNode> runtimeTypeAdapterFactory = RuntimeTypeAdapterFactory
                .of(TranspilerNode.class, "type")
                .registerSubtype(NegativeNode.class, "negative")
                .registerSubtype(ExponentNode.class, "exponent")
                .registerSubtype(AddNode.class, "add")
                .registerSubtype(SubtractNode.class, "subtract")
                .registerSubtype(MultiplyNode.class, "multiply")
                .registerSubtype(DivideNode.class, "divide")
                .registerSubtype(IfTranspilerNode.class, "if")
                .registerSubtype(EqualToNode.class, "equal_to")
                .registerSubtype(NotEqualToNode.class, "not_equal_to")
                .registerSubtype(GreaterThanNode.class, "greater_than")
                .registerSubtype(LessThanNode.class, "less_than")
                .registerSubtype(GreaterThanOrEqualToNode.class, "greater_than_or_equal_to")
                .registerSubtype(LessThanOrEqualToNode.class, "less_than_or_equal_to")
                .registerSubtype(FunctionCallNode.class, "function_call")
                .registerSubtype(IndexReferenceNode.class, "index_reference")
                .registerSubtype(JumpTranspilerNode.class, "jump")
                .registerSubtype(ValueOfNode.class, "valueOf")
                .registerSubtype(TranspilerLoop.class, "loop")
                .registerSubtype(TranspilerLoopUpdate.class, "loop_update")
                .registerSubtype(ListIterationTranspilerNode.class, "list_iterate")
                .registerSubtype(ExitIterationScopeLocationNode.class, "break")
                .registerSubtype(TranspilerCodeBlock.class, "block")
                .registerSubtype(SymbolReferenceNode.class, "symbol_reference")
                .registerSubtype(LabelledTranspilerCodeBlockNode.class, "labelled_block")
                .registerSubtype(NestedConditionNode.class, "nested_condition")
                .registerSubtype(NextLocationNode.class, "next_location")
                .registerSubtype(SetTranspilerNode.class, "set")
                .registerSubtype(PrintTranspilerNode.class, "print")
                .registerSubtype(AndNode.class, "and")
                .registerSubtype(OrNode.class, "or")
                .registerSubtype(NotNode.class, "not")
                .registerSubtype(PrimitiveValueNode.class, "primitive")
                .registerSubtype(ProgramTerminalLocationNode.class, "terminal_location")
                .registerSubtype(PlaceholderTranspilerNode.class, "placeholder")
                .registerSubtype(NullTranspilerNode.class, "null_node")
                .registerSubtype(ExitTranspilerNode.class, "exit")
                ;
        return new GsonBuilder().setPrettyPrinting().registerTypeAdapterFactory(runtimeTypeAdapterFactory).create();
    }
}
