package org.smojol.toolkit.analysis.defined;

import com.mojo.woof.EdgeType;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.neo4j.driver.Record;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeLike;
import org.smojol.common.ast.InstructionEdge;
import org.smojol.common.pseudocode.*;
import org.smojol.toolkit.analysis.graph.NamespaceQualifier;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.NodeToWoof;
import org.smojol.toolkit.task.*;

import java.util.ArrayList;
import java.util.List;

public class AnalyseControlFlowTask implements AnalysisTask {
    private final FlowNode astRoot;
    private final BasicBlockFactory basicBlockFactory;
    private final Neo4JDriverBuilder neo4JDriverBuilder;

    public AnalyseControlFlowTask(FlowNode astRoot, BasicBlockFactory basicBlockFactory, Neo4JDriverBuilder neo4JDriverBuilder) {
        this.astRoot = astRoot;
        this.basicBlockFactory = basicBlockFactory;
        this.neo4JDriverBuilder = neo4JDriverBuilder;
    }

    @Override
    public AnalysisTaskResult run() {
        AnalysisTaskResult result = new BuildPseudocodeGraphTask(astRoot, true).run();
        return switch (result) {
            case AnalysisTaskResultError analysisTaskResultError -> analysisTaskResultError;
            case AnalysisTaskResultOK analysisTaskResultOK -> analyse(analysisTaskResultOK.getDetail());
        };
    }

    private AnalysisTaskResult analyse(PseudocodeGraph graph) {
        List<BasicBlock> basicBlocks = basicBlocks(graph);
        return new AnalysisTaskResultOK(CommandLineAnalysisTask.ANALYSE_CONTROL_FLOW.name(), ImmutablePair.of(basicBlocks, graph));
    }

    private List<BasicBlock> basicBlocks(PseudocodeGraph graph) {
        BasicBlock currentBlock = basicBlockFactory.block();
        List<BasicBlock> stack = new ArrayList<>();
        for (int i = 0; i < graph.instructions().size(); i++) {
            PseudocodeInstruction instruction = graph.instructions().get(i);
            if (isJoinPoint(instruction, graph)) {
                if (currentBlock.isEmpty()) currentBlock.add(instruction);
                else {
                    stack.add(currentBlock);
                    currentBlock = basicBlockFactory.block();
                    currentBlock.add(instruction);
                }
            } else if (isBranchPoint(instruction, graph)) {
                currentBlock.add(instruction);
                stack.add(currentBlock);
                currentBlock = basicBlockFactory.block();
            } else if (instruction.isJump() && instruction.codeSentinelType() == CodeSentinelType.BODY) {
                currentBlock.add(instruction);
                stack.add(currentBlock);
                currentBlock = basicBlockFactory.block();
            } else if (instruction.isCondition() && instruction.codeSentinelType() == CodeSentinelType.BODY) {
                currentBlock.add(instruction);
                stack.add(currentBlock);
                currentBlock = basicBlockFactory.block();
            } else currentBlock.add(instruction);
        }
        stack.add(currentBlock);
        return stack;
    }

    private boolean isBranchPoint(PseudocodeInstruction instruction, PseudocodeGraph graph) {
        return graph.edges().stream().filter(e -> e.getFrom() == instruction).count() > 1;
    }

    private boolean isJoinPoint(PseudocodeInstruction instruction, PseudocodeGraph graph) {
        return graph.edges().stream().filter(e -> e.getTo() == instruction).count() > 1;
    }

    private void injectIntoNeo4J(List<FlowNodeLike> nodes, List<InstructionEdge> edges) {
        GraphSDK graphSDK = new GraphSDK(neo4JDriverBuilder.fromEnv());
        NodeSpecBuilder specBuilder = new NodeSpecBuilder(new NamespaceQualifier("SOME"));
        nodes.forEach(n -> graphSDK.createNode(NodeToWoof.toWoofNode(n, specBuilder)));
        edges.forEach(e -> {
            PseudocodeInstruction from = e.getFrom();
            PseudocodeInstruction to = e.getTo();
            Record recordFrom = NodeToWoof.existingCFGNode(from, specBuilder, graphSDK);
            Record recordTo = NodeToWoof.existingCFGNode(to, specBuilder, graphSDK);
            graphSDK.connect(recordFrom, recordTo, e.getEdgeType().name(), EdgeType.FLOW);
        });
    }
}
