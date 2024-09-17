package org.smojol.toolkit.analysis.defined;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.TranspilerInstructionGeneratorVisitor;
import org.smojol.common.navigation.AggregatingTranspilerNodeTraversal;
import org.smojol.common.pseudocode.*;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.toolkit.task.*;
import org.smojol.toolkit.transpiler.TranspilerTreeBuilder;

import java.util.List;

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
        TranspilerInstructionGeneratorVisitor visitor = new TranspilerInstructionGeneratorVisitor(new IncrementingIdProvider());
        new AggregatingTranspilerNodeTraversal<List<TranspilerInstruction>>().accept(transpilerTree, visitor);
        System.out.println(visitor.result());
        return new AnalysisTaskResultOK(CommandLineAnalysisTask.ANALYSE_CONTROL_FLOW.name(), transpilerTree);
    }
}
