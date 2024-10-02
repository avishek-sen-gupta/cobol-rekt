package org.smojol.common.transpiler;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.pseudocode.BasicBlock;

import java.util.List;

public record TranspilerFlowgraph(Graph<BasicBlock<TranspilerInstruction>, DefaultEdge> basicBlockFlowgraph,
                                  Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph,
                                  TranspilerNode transpilerTree, List<TranspilerInstruction> instructions,
                                  List<BasicBlock<TranspilerInstruction>> basicBlocks) {
}
