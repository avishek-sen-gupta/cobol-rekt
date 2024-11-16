package org.smojol.common.transpiler;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;

public record ProcedureRange(TranspilerInstruction entry,
                             TranspilerInstruction exit,
                             Graph<TranspilerInstruction, DefaultEdge> body) {
    @Override
    public String toString() {
        return entry.label() + " -> " + exit.label();
    }
}
