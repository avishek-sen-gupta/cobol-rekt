package com.mojo.algorithms.task;

import com.mojo.algorithms.domain.*;
import com.mojo.algorithms.transpiler.*;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.stream.Collectors;


public class ReachingConditionDefinitionTask<V extends TranspilerInstruction, E> {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(ReachingConditionDefinitionTask.class.getName());
    private final GraphSlice<V, E> slice;

    public ReachingConditionDefinitionTask(GraphSlice<V, E> slice) {
        this.slice = slice;
    }

    public Map<V, TranspilerNode> run() {
        Graph<V, E> graph = slice.sourceGraph();
        List<V> orderedVertices = slice.topologicallyOrderedVertices();
        Map<V, TranspilerNode> chainConditionMap = new HashMap<>();
        Map<E, TranspilerNode> edgeConditionMap = graph.edgeSet().stream().map(edge -> (Pair<E, TranspilerNode>) ImmutablePair.of(edge, conditionExpression(edge, graph)))
                .collect(Collectors.toMap(Pair::getKey, Pair::getValue));
        for (V vertex : orderedVertices) {
            TranspilerNode aggregateCondition = graph.incomingEdgesOf(vertex).stream()
                    .map(edge -> resolvedAnd(edgeConditionMap.get(edge), chainCondition(graph.getEdgeSource(edge), chainConditionMap)))
                    .reduce(new PrimitiveValueTranspilerNode(TypedRecord.FALSE), this::resolvedOr);
            chainConditionMap.put(vertex, aggregateCondition);
        }

        return chainConditionMap;

    }

    private TranspilerNode resolvedAnd(TranspilerNode lhs, TranspilerNode rhs) {
        if (isFalsePrimitive(lhs) || isFalsePrimitive(rhs)) return lhs;
        else if (isTruePrimitive(lhs)) return rhs;
        else if (isTruePrimitive(rhs)) return lhs;
        return new AndTranspilerNode(lhs, rhs);
    }

    private TranspilerNode resolvedOr(TranspilerNode lhs, TranspilerNode rhs) {
        if (isTruePrimitive(lhs) || isTruePrimitive(rhs))
            return new PrimitiveValueTranspilerNode(TypedRecord.TRUE);
        else if (isFalsePrimitive(lhs)) return rhs;
        else if (isFalsePrimitive(rhs)) return lhs;
        return new OrTranspilerNode(lhs, rhs);
    }

    private TranspilerNode chainCondition(V vertex, Map<V, TranspilerNode> chainConditionMap) {
        if (!chainConditionMap.containsKey(vertex)) {
            PrimitiveValueTranspilerNode alwaysTrue = new PrimitiveValueTranspilerNode(TypedRecord.TRUE);
            chainConditionMap.put(vertex, alwaysTrue);
            LOGGER.info("Possibly isolated pruneable node: " + vertex.description());
            return alwaysTrue;
        }
        return chainConditionMap.get(vertex);
    }

    private boolean isTruePrimitive(TranspilerNode node) {
        return node instanceof PrimitiveValueTranspilerNode n && n.getValue().equals(TypedRecord.TRUE);
    }

    private boolean isFalsePrimitive(TranspilerNode node) {
        return node instanceof PrimitiveValueTranspilerNode n && n.getValue().equals(TypedRecord.FALSE);
    }

    private TranspilerNode conditionExpression(E edge, Graph<V, E> graph) {
        V edgeSource = graph.getEdgeSource(edge);
        if (!(edge instanceof AnnotatedEdge annotatedEdge)) {
            LOGGER.warning("NO ANNOTATED EDGE " + edge);
            return new PrimitiveValueTranspilerNode(TypedRecord.TRUE);
        }
        if ("THEN_ENTRY".equals(annotatedEdge.data("edgeType")))
            return ((IfTranspilerNode) edgeSource.ref()).getCondition();
        if ("ELSE_ENTRY".equals(annotatedEdge.data("edgeType")))
            return new NotTranspilerNode(((IfTranspilerNode) edgeSource.ref()).getCondition());
        return new PrimitiveValueTranspilerNode(TypedRecord.TRUE);
    }
}
