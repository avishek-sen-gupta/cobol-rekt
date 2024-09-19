package org.smojol.common.transpiler;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.ast.FlowNodeType;

import java.util.ArrayList;
import java.util.List;

public record TranspilerModel(TranspilerNode tree, List<TranspilerInstruction> instructions,
                              List<TranspilerEdge> instructionEdges) {
    public void pruneUnreachables(Graph<TranspilerInstruction, DefaultEdge> jgraph) {
        do {
            System.out.println("Pruning...");
        } while (pruneOneRound(jgraph));
    }

    private static boolean pruneOneRound(Graph<TranspilerInstruction, DefaultEdge> jgraph) {
        boolean modified = false;
        List<TranspilerInstruction> allNodes = new ArrayList<>(jgraph.vertexSet());
        for (TranspilerInstruction node : allNodes) {
            if (FlowNodeType.PROCEDURE_DIVISION_BODY.equals(node.ref().getProperty("type"))
                    || jgraph.incomingEdgesOf(node).stream().distinct().count() > 0) continue;
            jgraph.removeVertex(node);
            modified = true;
        }

        return modified;
    }
}
