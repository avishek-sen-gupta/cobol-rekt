package org.smojol.toolkit.analysis.defined;

import com.mojo.woof.EdgeType;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import com.mojo.woof.NodeRelations;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.neo4j.driver.Record;
import org.smojol.common.ast.FlowNodeLike;
import org.smojol.common.pseudocode.*;
import org.smojol.common.transpiler.TranspilerInstructionEdge;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.common.transpiler.TranspilerModel;
import org.smojol.toolkit.analysis.graph.NamespaceQualifier;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.NodeToWoof;
import org.smojol.toolkit.task.*;

import java.util.ArrayList;
import java.util.List;

public class AnalyseBasicBlocksTask implements AnalysisTask {
    private final BasicBlockFactory<TranspilerInstruction> basicBlockFactory;
    private final Neo4JDriverBuilder neo4JDriverBuilder;
    private final TranspilerModel model;

    public AnalyseBasicBlocksTask(TranspilerModel model, BasicBlockFactory<TranspilerInstruction> basicBlockFactory, Neo4JDriverBuilder neo4JDriverBuilder) {
        this.model = model;
        this.basicBlockFactory = basicBlockFactory;
        this.neo4JDriverBuilder = neo4JDriverBuilder;
    }

    @Override
    public AnalysisTaskResult run() {
        List<BasicBlock<TranspilerInstruction>> basicBlocks = basicBlocks(model);
        return new AnalysisTaskResultOK(CommandLineAnalysisTask.BASIC_BLOCKS_TASK.name(), ImmutablePair.of(basicBlocks, model));
    }

    private List<BasicBlock<TranspilerInstruction>> basicBlocks(TranspilerModel model) {
        List<TranspilerInstruction> instructions = model.instructions();
        BasicBlock<TranspilerInstruction> currentBlock = basicBlockFactory.block();
        List<BasicBlock<TranspilerInstruction>> stack = new ArrayList<>();
        for (TranspilerInstruction instruction : instructions) {
            if (isJoinPoint(instruction)) {
                if (currentBlock.isEmpty()) currentBlock.add(instruction);
                else {
                    stack.add(currentBlock);
                    currentBlock = basicBlockFactory.block();
                    currentBlock.add(instruction);
                }
            } else if (isBranchPoint(instruction)) {
                currentBlock.add(instruction);
                stack.add(currentBlock);
                currentBlock = basicBlockFactory.block();
            } else if (instruction.isJump() && instruction.sentinel() == CodeSentinelType.BODY) {
                currentBlock.add(instruction);
                stack.add(currentBlock);
                currentBlock = basicBlockFactory.block();
            } else if (instruction.isCondition() && instruction.sentinel() == CodeSentinelType.BODY) {
                currentBlock.add(instruction);
                stack.add(currentBlock);
                currentBlock = basicBlockFactory.block();
            } else currentBlock.add(instruction);
        }
        stack.add(currentBlock);
        return stack;
    }

    private boolean isBranchPoint(TranspilerInstruction instruction) {
        return model.jgraph().outgoingEdgesOf(instruction).size() > 1;
    }

    private boolean isJoinPoint(TranspilerInstruction instruction) {
        return model.jgraph().incomingEdgesOf(instruction).size() > 1;
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
