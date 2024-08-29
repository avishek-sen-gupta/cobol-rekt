package org.smojol.toolkit.analysis.defined;

import org.smojol.common.ast.*;
import org.smojol.common.id.IdProvider;
import org.smojol.common.pseudocode.PseudocodeGeneratorVisitor;
import org.smojol.common.pseudocode.PseudocodeInstruction;
import org.smojol.toolkit.interpreter.navigation.AggregatingFlowNodeASTTraversal;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

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
        return AnalysisTaskResult.OK(CommandLineAnalysisTask.BUILD_PSEUDOCODE, instructions);
    }
}
