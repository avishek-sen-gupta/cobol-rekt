package org.smojol.common.transpiler;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;

import java.util.Set;

public record ProcedureRange(TranspilerInstruction entry,
                             TranspilerInstruction exit,
                             Graph<TranspilerInstruction, DefaultEdge> body,
                             Set<ProcedureRange> childRanges) {
}
