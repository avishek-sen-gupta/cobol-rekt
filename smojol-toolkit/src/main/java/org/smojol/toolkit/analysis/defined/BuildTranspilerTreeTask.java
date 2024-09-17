package org.smojol.toolkit.analysis.defined;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeSymbolExtractorVisitor;
import org.smojol.common.pseudocode.*;
import org.smojol.common.transpiler.TranspilerSetup;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.ast.FlowNodeASTTraversal;
import org.smojol.toolkit.task.*;
import org.smojol.toolkit.transpiler.TranspilerTreeBuilder;

public class BuildTranspilerTreeTask implements AnalysisTask {
    private final FlowNode astRoot;
    private final CobolDataStructure dataStructures;
    private final SmojolSymbolTable symbolTable;

    public BuildTranspilerTreeTask(FlowNode astRoot, CobolDataStructure dataStructures, SmojolSymbolTable symbolTable) {
        this.astRoot = astRoot;
        this.dataStructures = dataStructures;
        this.symbolTable = symbolTable;
    }

    @Override
    public AnalysisTaskResult run() {
//        TranspilerSetup.buildSymbolTable(astRoot, dataStructures, symbolTable);
        TranspilerNode transpilerTree = TranspilerTreeBuilder.flowToTranspiler(astRoot, dataStructures);
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
