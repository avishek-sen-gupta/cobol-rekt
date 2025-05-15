package com.mojo.algorithms.task;

import com.mojo.algorithms.domain.TranspilerFlowgraph;
import org.jgrapht.Graph;

import java.util.List;
import java.util.function.Function;
import java.util.logging.Logger;

public class PruneUnreachableTask {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(PruneUnreachableTask.class.getName());
    public static <V, E> void pruneUnreachables(Graph<V, E> blockGraph, Function<V, Boolean> isRoot) {
        while (pruneUnreachablesInternal(blockGraph, isRoot)) {
            LOGGER.info("PRUNED...");
        }
    }

    private static <V, E> boolean pruneUnreachablesInternal(Graph<V, E> blockGraph, Function<V, Boolean> isRoot) {
        List<V> verticesToRemove = blockGraph.vertexSet().stream()
                .filter(v -> !isRoot.apply(v) && blockGraph.incomingEdgesOf(v).isEmpty())
                .toList();
        verticesToRemove.forEach(blockGraph::removeVertex);
        return !verticesToRemove.isEmpty();
    }

    public static void pruneUnreachableInstructions(TranspilerFlowgraph transpilerFlowgraph) {
        pruneUnreachables(transpilerFlowgraph.instructionFlowgraph(), instr -> instr == transpilerFlowgraph.instructions().getFirst());
    }
}
