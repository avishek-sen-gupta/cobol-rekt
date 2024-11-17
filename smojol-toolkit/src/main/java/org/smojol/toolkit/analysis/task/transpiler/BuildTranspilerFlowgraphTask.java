package org.smojol.toolkit.analysis.task.transpiler;

import com.google.common.collect.ImmutableList;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import org.antlr.v4.runtime.tree.ParseTree;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.neo4j.driver.Record;
import org.smojol.common.flowchart.MermaidGraph;
import org.smojol.common.id.IncrementingIdProvider;
import org.smojol.common.pseudocode.BasicBlock;
import org.smojol.common.pseudocode.BasicBlockFactory;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.common.transpiler.*;
import org.smojol.common.typeadapter.RuntimeTypeAdapterFactory;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.graph.NamespaceQualifier;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.NodeToWoof;
import org.smojol.toolkit.analysis.pipeline.config.OutputArtifactConfig;
import org.smojol.toolkit.transpiler.TranspilerLoopUpdate;

import java.util.List;
import java.util.Set;

public class BuildTranspilerFlowgraphTask {
    private final ParseTree rawAST;
    private final CobolDataStructure dataStructures;
    private final SmojolSymbolTable symbolTable;
    private final OutputArtifactConfig transpilerModelOutputConfig;
    private final ResourceOperations resourceOperations;
    private final Neo4JDriverBuilder neo4JDriverBuilder;
    private final List<String> flowHints;

    public BuildTranspilerFlowgraphTask(ParseTree rawAST, CobolDataStructure dataStructures, SmojolSymbolTable symbolTable, List<String> flowHints, OutputArtifactConfig transpilerModelOutputConfig, ResourceOperations resourceOperations, Neo4JDriverBuilder neo4JDriverBuilder) {
        this.rawAST = rawAST;
        this.dataStructures = dataStructures;
        this.symbolTable = symbolTable;
        this.transpilerModelOutputConfig = transpilerModelOutputConfig;
        this.resourceOperations = resourceOperations;
        this.neo4JDriverBuilder = neo4JDriverBuilder;
        this.flowHints = flowHints;
    }

    public BuildTranspilerFlowgraphTask(ParseTree rawAST, CobolDataStructure dataStructures, SmojolSymbolTable symbolTable, List<String> flowHints) {
        this(rawAST, dataStructures, symbolTable, flowHints, null, null, null);
    }

    public TranspilerFlowgraph run() {
        TranspilerNode transpilerTree = new BuildTranspilerASTTask(rawAST, dataStructures, symbolTable).run();

//        TranspilerNode mainNode = transpilerTree.findAllRecursive(t -> t instanceof LabelledTranspilerCodeBlockNode l && l.getName().equals("S-STEUERUNGS")).stream().findFirst().get();
        List<TranspilerInstruction> instructions = new BuildTranspilerInstructionsFromIntermediateTreeTask(transpilerTree, new IncrementingIdProvider()).run();
        Graph<TranspilerInstruction, DefaultEdge> implicitCFG = new BuildImplicitInstructionControlFlowgraphTask(instructions, ImmutableList.of()).run();
        Set<InvokingProcedureRange> rangesWithChildren = new ProcedureBodyTask(transpilerTree, instructions, implicitCFG).run();
        SLIFORangeCriterionTask task = new SLIFORangeCriterionTask(rangesWithChildren);
        Pair<Set<InvokingProcedureRange>, Set<InvokingProcedureRange>> categorisedRanges = task.run();

        Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph = new BuildInstructionFlowgraphTask(instructions, flowHints).run();
        Pair<Graph<BasicBlock<TranspilerInstruction>, DefaultEdge>, List<BasicBlock<TranspilerInstruction>>> basicBlockModel = new BuildBasicBlocksTask(instructions, instructionFlowgraph, new BasicBlockFactory<>(new IncrementingIdProvider()), neo4JDriverBuilder).run();
        Graph<BasicBlock<TranspilerInstruction>, DefaultEdge> basicBlockGraph = basicBlockModel.getLeft();
        MermaidGraph<TranspilerInstruction, DefaultEdge> mermaid = new MermaidGraph<>();
        String draw = mermaid.draw(instructionFlowgraph);
        return new TranspilerFlowgraph(basicBlockGraph, instructionFlowgraph, transpilerTree, instructions, basicBlockModel.getRight(), categorisedRanges);

//        try {
//            resourceOperations.createDirectories(transpilerModelOutputConfig.outputDir());
//        } catch (IOException e) {
//            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.BUILD_TRANSPILER_FLOWGRAPH);
//        }
//
//        try (JsonWriter writer = new JsonWriter(resourceOperations.fileWriter(transpilerModelOutputConfig.fullPath()))) {
//            Gson gson = initGson();
//            writer.setIndent("  ");
//            gson.toJson(instructionFlowgraph, TranspilerInstructionModel.class, writer);
//            String draw = mermaid.draw(instructionFlowgraph.instructionFlowgraph());
////            injectIntoNeo4J(instructionFlowgraph.tree());
//            return AnalysisTaskResult.OK(CommandLineAnalysisTask.BUILD_TRANSPILER_FLOWGRAPH, new TranspilerFlowgraph(basicBlockGraph, instructionFlowgraph));
//        } catch (IOException e) {
//            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.BUILD_TRANSPILER_FLOWGRAPH);
//        }
    }


    private void injectIntoNeo4J(TranspilerNode current) {
        GraphSDK graphSDK = new GraphSDK(neo4JDriverBuilder.fromEnv());
        NodeSpecBuilder specBuilder = new NodeSpecBuilder(new NamespaceQualifier("SOME"));
        recursivelyInject(current, graphSDK, specBuilder);
    }

    private Record recursivelyInject(TranspilerNode current, GraphSDK graphSDK, NodeSpecBuilder specBuilder) {
        System.out.println("CREATING " + current);
        Record nodeEntry = graphSDK.createNode(NodeToWoof.toWoofNode(current, specBuilder));
        List<Record> childRecords = current.astChildren().stream().map(c -> recursivelyInject(c, graphSDK, specBuilder)).toList();
        List<Record> internalRecords = current.internalElements().stream().map(c -> recursivelyInject(c, graphSDK, specBuilder)).toList();
        for (Record child : childRecords) {
            graphSDK.connect(nodeEntry, child, "CONTAINS", "TRANSPILER_AST");
        }
        for (Record child : internalRecords) {
            graphSDK.connect(nodeEntry, child, "CONTAINS", "INTERNAL_ELEMENT");
        }
        return nodeEntry;
    }

    public static Gson initGson() {
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
                .registerSubtype(TranspilerCodeBlockNode.class, "block")
                .registerSubtype(DetachedTranspilerCodeBlockNode.class, "detachedBlock")
                .registerSubtype(SymbolReferenceNode.class, "symbol_reference")
                .registerSubtype(LabelledTranspilerCodeBlockNode.class, "labelled_block")
                .registerSubtype(NestedConditionNode.class, "nested_condition")
                .registerSubtype(NextLocationNode.class, "next_location")
                .registerSubtype(SetTranspilerNode.class, "set")
                .registerSubtype(PrintTranspilerNode.class, "print")
                .registerSubtype(AndTranspilerNode.class, "and")
                .registerSubtype(OrTranspilerNode.class, "or")
                .registerSubtype(NotTranspilerNode.class, "not")
                .registerSubtype(PrimitiveValueTranspilerNode.class, "primitive")
                .registerSubtype(ProgramTerminalLocationNode.class, "terminal_location")
                .registerSubtype(PlaceholderTranspilerNode.class, "placeholder")
                .registerSubtype(NullTranspilerNode.class, "null_node")
                .registerSubtype(ExitTranspilerNode.class, "exit");
        return new GsonBuilder().setPrettyPrinting().registerTypeAdapterFactory(runtimeTypeAdapterFactory).create();
    }
}
