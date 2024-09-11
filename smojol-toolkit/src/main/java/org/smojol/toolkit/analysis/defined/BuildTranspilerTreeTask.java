package org.smojol.toolkit.analysis.defined;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.pseudocode.*;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.toolkit.task.*;
import org.smojol.toolkit.transpiler.TranspilerCodeBlock;
import org.smojol.toolkit.transpiler.TranspilerTreeBuilder;

import java.util.List;

public class BuildTranspilerTreeTask implements AnalysisTask {
    private final FlowNode astRoot;
    private final CobolDataStructure dataStructures;

    public BuildTranspilerTreeTask(FlowNode astRoot, CobolDataStructure dataStructures) {
        this.astRoot = astRoot;
        this.dataStructures = dataStructures;
    }

    @Override
    public AnalysisTaskResult run() {
        AnalysisTaskResult result = new BuildPseudocodeGraphTask(astRoot).run();
        return switch (result) {
            case AnalysisTaskResultError analysisTaskResultError -> analysisTaskResultError;
            case AnalysisTaskResultOK analysisTaskResultOK -> analyse(analysisTaskResultOK.getDetail());
        };
    }

    private AnalysisTaskResult analyse(PseudocodeGraph graph) {
        List<TranspilerNode> stuff = graph.instructions().stream().filter(instr -> instr.codeSentinelType() == CodeSentinelType.BODY).map(instr -> TranspilerTreeBuilder.flowToTranspiler(instr.getNode(), dataStructures)).toList();
        List<TranspilerNode> nonEmptyInstructions = stuff.stream().filter(instr -> !(instr instanceof TranspilerCodeBlock && ((TranspilerCodeBlock) instr).isEmpty())).toList();
        return new AnalysisTaskResultOK(CommandLineAnalysisTask.ANALYSE_CONTROL_FLOW.name(), graph);
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
