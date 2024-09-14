package org.smojol.toolkit.analysis.defined;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.pseudocode.*;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.toolkit.task.*;
import org.smojol.toolkit.transpiler.TranspilerTreeBuilder;

public class BuildTranspilerTreeTask implements AnalysisTask {
    private final FlowNode astRoot;
    private final CobolDataStructure dataStructures;

    public BuildTranspilerTreeTask(FlowNode astRoot, CobolDataStructure dataStructures) {
        this.astRoot = astRoot;
        this.dataStructures = dataStructures;
    }

    @Override
    public AnalysisTaskResult run() {
        TranspilerNode transpilerTree = TranspilerTreeBuilder.flowToTranspiler(astRoot, dataStructures, CodeSentinelType.BODY);
        return new AnalysisTaskResultOK(CommandLineAnalysisTask.ANALYSE_CONTROL_FLOW.name(), transpilerTree);
    }
}

/*
Transpiler Building Blocks:
    - block
        - statements+
    - ref(expression, [exp1, exp2, exp3, ...])
    - ref(expression)

    - value(ref)

    - set(ref, expression)
    - expression = multdivs (+ )

    - if (condition) then ...
        else if ...
        else ...
    - while (condition)
        block
    - next_instruction_id(expression)
 */
