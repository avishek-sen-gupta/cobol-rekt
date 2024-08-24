package org.smojol.toolkit.analysis.defined;

import org.smojol.common.ast.AggregatingFlowNodeASTVisitor;
import org.smojol.common.ast.PseudocodeGeneratorVisitor;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.PseudocodeInstruction;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.interpreter.navigation.AggregatingFlowNodeASTTraversal;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.util.List;

public class BuildPseudocodeTask implements AnalysisTask {
    private final CobolDataStructure dataStructures;
    private final FlowNode flowRoot;

    public BuildPseudocodeTask(FlowNode flowRoot, CobolDataStructure dataStructures) {
        this.dataStructures = dataStructures;
        this.flowRoot = flowRoot;
    }

    @Override
    public AnalysisTaskResult run() {
        AggregatingFlowNodeASTVisitor<List<PseudocodeInstruction>> visitor = new PseudocodeGeneratorVisitor(null);
        new AggregatingFlowNodeASTTraversal<List<PseudocodeInstruction>>().accept(flowRoot, visitor);
        return AnalysisTaskResult.OK(CommandLineAnalysisTask.BUILD_PSEUDO_BYTECODE, visitor.result());
    }
}
