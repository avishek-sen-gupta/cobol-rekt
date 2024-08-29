package org.smojol.toolkit.analysis.defined;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.smojol.common.ast.*;
import org.smojol.common.id.IdProvider;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.navigation.PseudocodeNavigator;
import org.smojol.common.pseudocode.PseudocodeGeneratorVisitor;
import org.smojol.common.pseudocode.PseudocodeInstruction;
import org.smojol.common.pseudocode.PseudocodeMetatype;
import org.smojol.toolkit.interpreter.navigation.AggregatingFlowNodeASTTraversal;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.util.ArrayList;
import java.util.List;

public class BuildPseudocodeTask implements AnalysisTask {
    private final FlowNode flowRoot;
    private final IdProvider uuidProvider;

    public BuildPseudocodeTask(FlowNode flowRoot, IdProvider uuidProvider) {
        this.flowRoot = flowRoot;
        this.uuidProvider = uuidProvider;
    }

    @Override
    public AnalysisTaskResult run() {
        AggregatingFlowNodeASTVisitor<List<PseudocodeInstruction>> visitor = new PseudocodeGeneratorVisitor(null, uuidProvider);
        new AggregatingFlowNodeASTTraversal<List<PseudocodeInstruction>>().accept(flowRoot, visitor);
        List<PseudocodeInstruction> instructions = visitor.result();
        PseudocodeNavigator navigator = new PseudocodeNavigator(flowRoot);
        List<InstructionFlowNode> nodes = new ArrayList<>();
        List<InstructionEdge> edges = new ArrayList<>();
        instructions.forEach(instruction -> nodes.add(new InstructionFlowNode(instruction)));

        for (int i = 0; i < instructions.size() - 1; i++) {
            PseudocodeInstruction current = instructions.get(i);
            PseudocodeInstruction next = instructions.get(i + 1);
            if (current.isJump()) {
                List<ImmutablePair<PseudocodeInstruction, PseudocodeInstruction>> callTargets = navigator.findCallTargets(current, i, instructions);
                callTargets.forEach(ct -> edges.add(new InstructionEdge(current, ct.getLeft(), InstructionEdgeType.JUMP)));
                callTargets.forEach(ct -> edges.add(new InstructionEdge(ct.getRight(), current, InstructionEdgeType.RETURN)));
            } else if (current.isCondition()) {
                List<PseudocodeInstruction> childInstructions = current.getNode().astChildren().stream().map(ins -> navigator.findSingleByCondition(in -> in.getNode() == ins, instructions)).toList();
                PseudocodeInstruction convergencePoint = navigator.findSingleByCondition(instruction -> instruction.getNode() == current.getNode() && instruction.getMetatype() == PseudocodeMetatype.EXIT, instructions);
                childInstructions.forEach(c -> edges.add(new InstructionEdge(current, c, InstructionEdgeType.FOLLOWED_BY)));
                childInstructions.forEach(c -> edges.add(new InstructionEdge(c, convergencePoint, InstructionEdgeType.FOLLOWED_BY)));
            } else {
                edges.add(new InstructionEdge(current, next, InstructionEdgeType.FOLLOWED_BY));
            }
        }

        return AnalysisTaskResult.OK(CommandLineAnalysisTask.BUILD_PSEUDOCODE, instructions);
    }
}
