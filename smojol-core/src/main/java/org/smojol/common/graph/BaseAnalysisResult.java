package org.smojol.common.graph;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.structure.CobolDataStructure;

public record BaseAnalysisResult(ParseTree rawAST, CobolDataStructure dataStructures, SmojolSymbolTable symbolTable
) {
}
