package org.smojol.toolkit.analysis.defined;

import com.mojo.woof.Neo4JDriverBuilder;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.pseudocode.CodeSentinelType;
import org.smojol.common.pseudocode.PseudocodeGraph;
import org.smojol.common.pseudocode.PseudocodeInstruction;
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
        AnalysisTaskResult result = new BuildPseudocodeGraphTask(astRoot, neo4JDriverBuilder).run();
        return switch (result) {
            case AnalysisTaskResultError analysisTaskResultError -> analysisTaskResultError;
            case AnalysisTaskResultOK analysisTaskResultOK -> analyse(analysisTaskResultOK.getDetail());
        };
    }

    private AnalysisTaskResult analyse(PseudocodeGraph graph) {
        return new AnalysisTaskResultOK(CommandLineAnalysisTask.ANALYSE_CONTROL_FLOW.name(), basicBlocks(graph));
    }

    private List<BasicBlock> basicBlocks(PseudocodeGraph graph) {
        BasicBlock currentBlock = basicBlockFactory.block();
        List<BasicBlock> stack = new ArrayList<>();
        for (int i = 0; i < graph.instructions().size(); i++) {
            PseudocodeInstruction instruction = graph.instructions().get(i);
            if (isJoinPoint(instruction, graph)) {
                if (currentBlock.isEmpty()) {
                    currentBlock.add(instruction);
                } else {
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
}
