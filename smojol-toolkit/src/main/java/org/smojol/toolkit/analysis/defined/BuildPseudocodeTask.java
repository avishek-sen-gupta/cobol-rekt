package org.smojol.toolkit.analysis.defined;

import org.smojol.common.ast.*;
import org.smojol.common.id.IdProvider;
import org.smojol.common.pseudocode.PseudocodeGeneratorVisitor;
import org.smojol.common.pseudocode.PseudocodeInstruction;
import org.smojol.common.navigation.AggregatingFlowNodeASTTraversal;

import java.util.List;

public class BuildPseudocodeTask {
    private final FlowNode flowRoot;
    private final IdProvider uuidProvider;

    public BuildPseudocodeTask(FlowNode flowRoot, IdProvider uuidProvider) {
        this.flowRoot = flowRoot;
        this.uuidProvider = uuidProvider;
    }

    public List<PseudocodeInstruction> run() {
        AggregatingFlowNodeASTVisitor<List<PseudocodeInstruction>> visitor = new PseudocodeGeneratorVisitor(null, uuidProvider);
        new AggregatingFlowNodeASTTraversal<List<PseudocodeInstruction>>().accept(flowRoot, visitor);
        return visitor.result();
    }
}
