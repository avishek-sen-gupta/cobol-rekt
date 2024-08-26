package org.smojol.toolkit.analysis.defined;

import org.smojol.common.ast.AggregatingFlowNodeASTVisitor;
import org.smojol.common.pseudocode.PseudocodeGeneratorVisitor;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.pseudocode.PseudocodeInstruction;
import org.smojol.toolkit.interpreter.navigation.AggregatingFlowNodeASTTraversal;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.util.List;

public class BuildPseudocodeTask implements AnalysisTask {
    private final FlowNode flowRoot;

    public BuildPseudocodeTask(FlowNode flowRoot) {
        this.flowRoot = flowRoot;
    }

    @Override
    public AnalysisTaskResult run() {
        AggregatingFlowNodeASTVisitor<List<PseudocodeInstruction>> visitor = new PseudocodeGeneratorVisitor(null);
        new AggregatingFlowNodeASTTraversal<List<PseudocodeInstruction>>().accept(flowRoot, visitor);
        return AnalysisTaskResult.OK(CommandLineAnalysisTask.BUILD_PSEUDOCODE, visitor.result());
    }
}
