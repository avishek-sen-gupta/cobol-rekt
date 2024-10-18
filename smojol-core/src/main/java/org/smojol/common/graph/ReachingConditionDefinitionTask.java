package org.smojol.common.graph;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;

import java.util.*;
import java.util.stream.Stream;


public class ReachingConditionDefinitionTask<V extends TranspilerInstruction, E> {
    private final GraphSlice<V, E> slice;

    public ReachingConditionDefinitionTask(GraphSlice<V, E> slice) {
        this.slice = slice;
    }

    public Map<V, TranspilerNode> run() {
        Graph<V, E> gg = slice.inducedSubgraph();
        Map<E, TranspilerNode> edgeConditionMap = new HashMap<>();
        gg.edgeSet().stream().map(edge -> (Pair<E, TranspilerNode>) ImmutablePair.of(edge, conditionExpression(edge, gg)))
                .forEach(pair -> edgeConditionMap.put(pair.getLeft(), pair.getRight()));
        List<V> orderedVertices = slice.topologicallyOrderedVertices();
        Map<V, TranspilerNode> chainConditionMap = new HashMap<>();
        List<V> list = orderedVertices.stream().filter(v -> gg.incomingEdgesOf(v).size() > 2).toList();
        for (V vertex : orderedVertices) {
            TranspilerNode aggregateCondition = gg.incomingEdgesOf(vertex).stream()
                    .map(edge -> resolvedAnd(edgeConditionMap.get(edge), chainCondition(gg.getEdgeSource(edge), chainConditionMap)))
                    .reduce(new PrimitiveValueTranspilerNode(TypedRecord.FALSE), this::resolvedOr);
            chainConditionMap.put(vertex, aggregateCondition);
        }

        return chainConditionMap;

    }

    private TranspilerNode resolvedAnd(TranspilerNode lhs, TranspilerNode rhs) {
        if (isFalse(lhs) || isFalse(rhs)) return lhs;
        else if (isTrue(lhs)) return rhs;
        else if (isTrue(rhs)) return lhs;
        return new AndTranspilerNode(lhs, rhs);
    }

    private TranspilerNode resolvedOr(TranspilerNode lhs, TranspilerNode rhs) {
        if (isTrue(lhs) || isTrue(rhs)) return lhs;
        else if (isFalse(lhs)) return rhs;
        else if (isFalse(rhs)) return lhs;
        return new OrTranspilerNode(lhs, rhs);
    }

    private TranspilerNode chainCondition(V vertex, Map<V, TranspilerNode> chainConditionMap) {
        if (!chainConditionMap.containsKey(vertex)){
            PrimitiveValueTranspilerNode alwaysTrue = new PrimitiveValueTranspilerNode(TypedRecord.TRUE);
            chainConditionMap.put(vertex, alwaysTrue);
            System.out.println("Possibly start node: " + vertex.description());
            return alwaysTrue;
        }
        return chainConditionMap.get(vertex);
    }

    private boolean isTrue(TranspilerNode node) {
        return node instanceof PrimitiveValueTranspilerNode n && n.getValue().equals(TypedRecord.TRUE);
    }

    private boolean isFalse(TranspilerNode node) {
        return node instanceof PrimitiveValueTranspilerNode n && n.getValue().equals(TypedRecord.FALSE);
    }

    private TranspilerNode conditionExpression(E edge, Graph<V, E> graph) {
        V edgeSource = graph.getEdgeSource(edge);
        if (!(edge instanceof AnnotatedEdge)) return new PrimitiveValueTranspilerNode(TypedRecord.TRUE);
//        if (edgeSource.sentinel() != BODY) return new PrimitiveValueTranspilerNode(TypedRecord.TRUE);
//        if (!(edgeSource.ref() instanceof IfTranspilerNode)) return new PrimitiveValueTranspilerNode(TypedRecord.TRUE);
        AnnotatedEdge annotatedEdge = (AnnotatedEdge) edge;
        if ("THEN_ENTRY".equals(annotatedEdge.data("edgeType")))
            return ((IfTranspilerNode) edgeSource.ref()).getCondition();
        if ("ELSE_ENTRY".equals(annotatedEdge.data("edgeType")))
            return new NotTranspilerNode(((IfTranspilerNode) edgeSource.ref()).getCondition());
        return new PrimitiveValueTranspilerNode(TypedRecord.TRUE);
    }
}
