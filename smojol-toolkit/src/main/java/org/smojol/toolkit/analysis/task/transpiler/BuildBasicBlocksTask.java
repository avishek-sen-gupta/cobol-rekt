package org.smojol.toolkit.analysis.task.transpiler;

import com.mojo.woof.EdgeType;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import com.mojo.woof.NodeRelations;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.neo4j.driver.Record;
import org.smojol.common.ast.FlowNodeLike;
import org.smojol.common.pseudocode.*;
import org.smojol.common.transpiler.PruneUnreachableTask;
import org.smojol.common.transpiler.TranspilerInstructionEdge;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.toolkit.analysis.graph.NamespaceQualifier;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.NodeToWoof;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public class BuildBasicBlocksTask {
    private final BasicBlockFactory<TranspilerInstruction> basicBlockFactory;
    private final Neo4JDriverBuilder neo4JDriverBuilder;
    private final Map<TranspilerInstruction, BasicBlock<TranspilerInstruction>> instructionToBlockMap = new HashMap<>();
    private final List<TranspilerInstruction> instructions;
    private final Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph;

    public BuildBasicBlocksTask(List<TranspilerInstruction> instructions, Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph, BasicBlockFactory<TranspilerInstruction> basicBlockFactory, Neo4JDriverBuilder neo4JDriverBuilder) {
        this.instructions = instructions;
        this.instructionFlowgraph = instructionFlowgraph;
        this.basicBlockFactory = basicBlockFactory;
        this.neo4JDriverBuilder = neo4JDriverBuilder;
    }

    public Graph<BasicBlock<TranspilerInstruction>, DefaultEdge> run() {
        List<BasicBlock<TranspilerInstruction>> basicBlocks = basicBlocks(instructions);
        Graph<BasicBlock<TranspilerInstruction>, DefaultEdge> blockGraph = new DefaultDirectedGraph<>(DefaultEdge.class);
        basicBlocks.forEach(blockGraph::addVertex);
        basicBlocks.forEach(bb -> {
            TranspilerInstruction instruction = bb.lastInstruction();
            instructionFlowgraph.outgoingEdgesOf(instruction).stream()
                    .map(instructionFlowgraph::getEdgeTarget)
                    .map(instructionToBlockMap::get)
                    .forEach(targetBB -> blockGraph.addEdge(bb, targetBB));
        });
        Function<BasicBlock<TranspilerInstruction>, Boolean> IS_ROOT = block -> block == basicBlocks.getFirst();
        PruneUnreachableTask.pruneUnreachables(blockGraph, IS_ROOT);
//        while (pruneUnreachables(blockGraph, IS_ROOT)) {
//            System.out.println("PRUNED...");
//        }

        return blockGraph;
    }

//    private static boolean pruneUnreachables(Graph<BasicBlock<TranspilerInstruction>, DefaultEdge> blockGraph, Function<BasicBlock<TranspilerInstruction>, Boolean> isRoot) {
//        List<BasicBlock<TranspilerInstruction>> verticesToRemove = blockGraph.vertexSet().stream()
//                .filter(v -> !isRoot.apply(v) && blockGraph.incomingEdgesOf(v).isEmpty())
//                .toList();
//        verticesToRemove.forEach(blockGraph::removeVertex);
//        return !verticesToRemove.isEmpty();
//    }

    private List<BasicBlock<TranspilerInstruction>> basicBlocks(List<TranspilerInstruction> instructions) {
        BasicBlock<TranspilerInstruction> currentBlock = basicBlockFactory.block();
        List<BasicBlock<TranspilerInstruction>> allBasicBlocks = new ArrayList<>();
        for (TranspilerInstruction instruction : instructions) {
            if (isJoinPoint(instruction)) {
                if (currentBlock.isEmpty()) {
                    addInstructionToCurrentBlock(instruction, currentBlock);
                }
                else {
                    addInstructionToCurrentBlock(instruction, currentBlock);
                    allBasicBlocks.add(currentBlock);
                    currentBlock = basicBlockFactory.block();
                }
            } else if (isBranchPoint(instruction)) {
                addInstructionToCurrentBlock(instruction, currentBlock);
                allBasicBlocks.add(currentBlock);
                currentBlock = basicBlockFactory.block();
            } else if (instruction.isJump() && instruction.sentinel() == CodeSentinelType.BODY) {
                addInstructionToCurrentBlock(instruction, currentBlock);
                allBasicBlocks.add(currentBlock);
                currentBlock = basicBlockFactory.block();
            } else if (instruction.isCondition() && instruction.sentinel() == CodeSentinelType.BODY) {
                addInstructionToCurrentBlock(instruction, currentBlock);
                allBasicBlocks.add(currentBlock);
                currentBlock = basicBlockFactory.block();
            } else {
                addInstructionToCurrentBlock(instruction, currentBlock);
            }
        }
        if (currentBlock.isEmpty()) return allBasicBlocks;
        allBasicBlocks.add(currentBlock);
        return allBasicBlocks;
    }

    private void addInstructionToCurrentBlock(TranspilerInstruction instruction, BasicBlock<TranspilerInstruction> currentBlock) {
        currentBlock.add(instruction);
        instructionToBlockMap.put(instruction, currentBlock);
    }

    private boolean isBranchPoint(TranspilerInstruction instruction) {
        return instructionFlowgraph.outgoingEdgesOf(instruction).size() > 1;
    }

    private boolean isJoinPoint(TranspilerInstruction instruction) {
        return instructionFlowgraph.incomingEdgesOf(instruction).size() > 1;
    }

    private void injectIntoNeo4J(List<FlowNodeLike> nodes, List<TranspilerInstructionEdge> edges) {
        GraphSDK graphSDK = new GraphSDK(neo4JDriverBuilder.fromEnv());
        NodeSpecBuilder specBuilder = new NodeSpecBuilder(new NamespaceQualifier("SOME"));
        nodes.forEach(n -> graphSDK.createNode(NodeToWoof.toWoofNode(n, specBuilder)));
        edges.forEach(e -> {
            TranspilerInstruction from = e.from();
            TranspilerInstruction to = e.to();
            Record recordFrom = NodeToWoof.existingCFGNode(from, specBuilder, graphSDK);
            Record recordTo = NodeToWoof.existingCFGNode(to, specBuilder, graphSDK);
            graphSDK.connect(recordFrom, recordTo, NodeRelations.JUMPS_TO, EdgeType.FLOW);
        });
    }
}
