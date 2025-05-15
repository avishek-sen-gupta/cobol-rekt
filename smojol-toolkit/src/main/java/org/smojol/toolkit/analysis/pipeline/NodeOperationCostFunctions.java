package org.smojol.toolkit.analysis.pipeline;


import com.mojo.algorithms.domain.TypedGraphVertex;

public record NodeOperationCostFunctions(
        java.util.function.ToDoubleFunction<TypedGraphVertex> add,
        java.util.function.ToDoubleFunction<TypedGraphVertex> remove,
        java.util.function.ToDoubleBiFunction<TypedGraphVertex, TypedGraphVertex> change) {

    public static NodeOperationCostFunctions GENERIC = new NodeOperationCostFunctions(
            v -> 5,
            v1 -> 5,
            (v2, w) -> v2.type().equals(w.type()) ? 0 : 1);
}
