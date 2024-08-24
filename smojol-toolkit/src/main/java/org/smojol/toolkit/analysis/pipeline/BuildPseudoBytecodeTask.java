package org.smojol.toolkit.analysis.pipeline;

import org.smojol.common.ast.BytecodeGeneratorVisitor;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.interpreter.navigation.FlowNodeASTTraversal;

public class BuildPseudoBytecodeTask implements Runnable {
    private final CobolDataStructure dataStructures;
    private final FlowNode flowRoot;

    public BuildPseudoBytecodeTask(FlowNode flowRoot, CobolDataStructure dataStructures) {
        this.dataStructures = dataStructures;
        this.flowRoot = flowRoot;
    }

    @Override
    public void run() {
        BytecodeGeneratorVisitor bytecodeGeneratorVisitor = new BytecodeGeneratorVisitor(null);
        new FlowNodeASTTraversal<FlowNode>().accept(flowRoot, bytecodeGeneratorVisitor);
    }
}
