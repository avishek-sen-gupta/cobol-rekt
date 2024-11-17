package org.smojol.common.transpiler;

import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.pseudocode.BasicBlock;

import java.util.List;
import java.util.Set;

public record TranspilerFlowgraph(Graph<BasicBlock<TranspilerInstruction>, DefaultEdge> basicBlockFlowgraph,
                                  Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph,
                                  TranspilerNode transpilerTree, List<TranspilerInstruction> instructions,
                                  List<BasicBlock<TranspilerInstruction>> basicBlocks,
                                  Pair<Set<InvokingProcedureRange>, Set<InvokingProcedureRange>> categorisedRanges) {
}
