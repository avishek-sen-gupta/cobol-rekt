package org.smojol.common.transpiler;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;

import java.util.List;

public record TranspilerInstructionModel(TranspilerNode tree, List<TranspilerInstruction> instructions,
                                         List<TranspilerInstructionEdge> instructionEdges,
                                         Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph) {
//    private static final java.util.logging.Logger LOGGER = Logger.getLogger(TranspilerInstructionModel.class.getName());

//    public void pruneUnreachables() {
//        PruneUnreachableTask.pruneUnreachables(instructionFlowgraph, instr -> instr == instructions.getFirst());
//        do {
//            LOGGER.info("Pruning...");
//        } while (pruneOneRound(instructionFlowgraph, instructions));
//    }

//    private static boolean pruneOneRound(Graph<TranspilerInstruction, DefaultEdge> jgraph, List<TranspilerInstruction> instructions) {
//        boolean modified = false;
//        List<TranspilerInstruction> allNodes = new ArrayList<>(jgraph.vertexSet());
//        for (TranspilerInstruction instr : allNodes) {
////            if (FlowNodeType.PROCEDURE_DIVISION_BODY.equals(instr.ref().getProperty("type"))
//            if (instr == instructions.getFirst()
//                    || jgraph.incomingEdgesOf(instr).stream().distinct().findAny().isPresent()) continue;
//            jgraph.removeVertex(instr);
//            modified = true;
//        }
//
//        return modified;
//    }
}
