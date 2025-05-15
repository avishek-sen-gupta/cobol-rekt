package com.mojo.algorithms.task;

import com.mojo.algorithms.domain.BasicBlock;
import com.mojo.algorithms.domain.BasicBlockFactory;
import com.mojo.algorithms.domain.CodeSentinelType;
import com.mojo.algorithms.domain.TranspilerInstruction;
import com.mojo.woof.Neo4JDriverBuilder;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

    public Pair<Graph<BasicBlock<TranspilerInstruction>, DefaultEdge>, List<BasicBlock<TranspilerInstruction>>> run() {
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
        return ImmutablePair.of(blockGraph, basicBlocks);
    }

    private List<BasicBlock<TranspilerInstruction>> basicBlocks(List<TranspilerInstruction> instructions) {
        BasicBlock<TranspilerInstruction> currentBlock = basicBlockFactory.block();
        List<BasicBlock<TranspilerInstruction>> allBasicBlocks = new ArrayList<>();
        for (TranspilerInstruction instruction : instructions) {
            if (isJoinPoint(instruction)) {
                if (currentBlock.isEmpty()) {
                    addInstructionToCurrentBlock(instruction, currentBlock);
                }
                else {
                    allBasicBlocks.add(currentBlock);
                    currentBlock = basicBlockFactory.block();
                    addInstructionToCurrentBlock(instruction, currentBlock);
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

//    private void injectIntoNeo4J(List<FlowNodeLike> nodes, List<TranspilerInstructionEdge> edges) {
//        GraphSDK graphSDK = new GraphSDK(neo4JDriverBuilder.fromEnv());
//        NodeSpecBuilder specBuilder = new NodeSpecBuilder(new NamespaceQualifier("SOME"));
//        nodes.forEach(n -> graphSDK.createNode(NodeToWoof.toWoofNode(n, specBuilder)));
//        edges.forEach(e -> {
//            TranspilerInstruction from = e.from();
//            TranspilerInstruction to = e.to();
//            Record recordFrom = NodeToWoof.existingCFGNode(from, specBuilder, graphSDK);
//            Record recordTo = NodeToWoof.existingCFGNode(to, specBuilder, graphSDK);
//            graphSDK.connect(recordFrom, recordTo, NodeRelations.JUMPS_TO, EdgeType.FLOW);
//        });
//    }
}
