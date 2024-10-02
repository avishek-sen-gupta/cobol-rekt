package org.smojol.toolkit.analysis.defined;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.intermediate.IntermediateASTNodeBuilder;
import org.smojol.toolkit.transpiler.TranspilerTreeBuilder;

public class BuildTranspilerASTTask {
    private final ParseTree rawAST;
    private final CobolDataStructure dataStructures;
    private final SmojolSymbolTable symbolTable;

    public BuildTranspilerASTTask(ParseTree rawAST, CobolDataStructure dataStructures, SmojolSymbolTable symbolTable) {
        this.rawAST = rawAST;
        this.dataStructures = dataStructures;
        this.symbolTable = symbolTable;
    }

    public TranspilerNode run() {
        FlowNode flowRoot = new IntermediateASTNodeBuilder(rawAST, dataStructures, symbolTable).build();
        return TranspilerTreeBuilder.flowToTranspiler(flowRoot, dataStructures);
    }
}
