package org.smojol.toolkit.analysis.task.transpiler;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.TranspilerInstructionGeneratorVisitor;
import org.smojol.common.id.IncrementingIdProvider;
import org.smojol.common.navigation.AggregatingTranspilerNodeTraversal;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.intermediate.IntermediateASTNodeBuilder;
import org.smojol.toolkit.transpiler.TranspilerTreeBuilder;

import java.util.List;

public class BuildTranspilerInstructionsFromRawASTTask {
    private final ParseTree rawAST;
    private final CobolDataStructure dataStructures;
    private final SmojolSymbolTable symbolTable;
    private final IncrementingIdProvider idProvider;

    public BuildTranspilerInstructionsFromRawASTTask(ParseTree rawAST, CobolDataStructure dataStructures, SmojolSymbolTable symbolTable) {
        this.rawAST = rawAST;
        this.dataStructures = dataStructures;
        this.symbolTable = symbolTable;
        idProvider = new IncrementingIdProvider();
    }

    public List<TranspilerInstruction> run() {
        FlowNode flowRoot = new IntermediateASTNodeBuilder(rawAST, dataStructures, symbolTable).build();
        TranspilerNode transpilerTree = TranspilerTreeBuilder.flowToTranspiler(flowRoot, dataStructures);
        return new BuildTranspilerInstructionsFromIntermediateTreeTask(transpilerTree, idProvider).run();
    }
}
