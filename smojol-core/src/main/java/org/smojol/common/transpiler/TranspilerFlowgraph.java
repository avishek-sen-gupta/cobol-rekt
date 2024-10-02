package org.smojol.common.transpiler;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.pseudocode.BasicBlock;

public record TranspilerFlowgraph(Graph<BasicBlock<TranspilerInstruction>, DefaultEdge> blockFlowgraph, TranspilerInstructionModel transpilerInstructionModel) {
}
