package org.smojol.common.graph;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.jooq.lambda.Seq;
import org.jooq.lambda.tuple.Tuple2;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static com.google.common.collect.Streams.zip;
import static org.smojol.common.pseudocode.CodeSentinelType.BODY;


public class ReachingConditionDefinitionTask<V extends TranspilerInstruction, E> {
    private final GraphSlice<V, E> slice;

    public ReachingConditionDefinitionTask(GraphSlice<V, E> slice) {
        this.slice = slice;
    }

    public Map<V, TranspilerNode> run3() {
        List<GraphPath<V, E>> paths = slice.allPaths();
        Map<E, TranspilerNode> edgeConditionMap = new HashMap<>();
        paths.stream()
                .flatMap(path -> path.getEdgeList().stream()
                        .map(edge -> (Pair<E, TranspilerNode>) ImmutablePair.of(edge, conditionExpression(path, edge))))
                .forEach(pair -> edgeConditionMap.put(pair.getLeft(), pair.getRight()));
        List<V> orderedVertices = slice.topologicallyOrderedVertices();
        Graph<V, E> graph = slice.inducedSubgraph();
        Map<V, TranspilerNode> chainConditionMap = new HashMap<>();
        List<V> list = orderedVertices.stream().filter(v -> graph.incomingEdgesOf(v).size() > 2).toList();
        for (V vertex : orderedVertices) {
            TranspilerNode aggregateCondition = graph.incomingEdgesOf(vertex).stream()
                    .map(edge -> resolvedAnd(edgeConditionMap.get(edge), chainCondition(graph.getEdgeSource(edge), chainConditionMap)))
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
        if (!chainConditionMap.containsKey(vertex))
            throw new RuntimeException("Edge " + vertex + " is not a chain condition");
        return chainConditionMap.get(vertex);
    }

    public Map<E, TranspilerNode> run2() {
        List<GraphPath<V, E>> paths = slice.allPaths();

        Map<E, TranspilerNode> edgeConditionMap = paths.stream()
                .flatMap(path -> path.getEdgeList().stream()
                        .map(edge -> (Pair<E, TranspilerNode>) ImmutablePair.of(edge, conditionExpression(path, edge))))
                .collect(Collectors.toMap(Pair::getLeft, Pair::getRight));

        List<Tuple2<E, TranspilerNode>> list = paths.stream()
                .map(path -> Seq.zip(path.getEdgeList(), Seq.<E, TranspilerNode>scanLeft(path.getEdgeList().stream(), new PrimitiveValueTranspilerNode(TypedRecord.TRUE),
                        (intermediateAcc, current) -> isTrue(intermediateAcc) ? edgeConditionMap.get(current) : resolvedAnd(edgeConditionMap.get(current), intermediateAcc))).toList())
                .flatMap(Collection::stream).toList();
        Set<Map.Entry<E, List<Tuple2<E, TranspilerNode>>>> pathConditionsPerVertex = list.stream().collect(Collectors.groupingBy(Tuple2::v1)).entrySet();
        Stream<TranspilerNode> sliceReachingConditions =
                pathConditionsPerVertex.stream().map(Map.Entry::getValue)
                        .map(perPathCondition -> perPathCondition.stream()
                                .map(Tuple2::v2)
                                .reduce(new PrimitiveValueTranspilerNode(TypedRecord.TRUE),
                                        (acc, currentPathCondition) -> new OrTranspilerNode(currentPathCondition, acc)));
        Map<E, TranspilerNode> zip = zip(pathConditionsPerVertex.stream().map(Map.Entry::getKey), sliceReachingConditions, ImmutablePair::of)
                .collect(Collectors.toMap(p -> p.getLeft(), p -> p.getRight()));
        return zip;
    }

    private boolean isTrue(TranspilerNode node) {
        return node instanceof PrimitiveValueTranspilerNode n && n.getValue().equals(TypedRecord.TRUE);
    }

    private boolean isFalse(TranspilerNode node) {
        return node instanceof PrimitiveValueTranspilerNode n && n.getValue().equals(TypedRecord.FALSE);
    }

    private TranspilerNode conditionExpression(GraphPath<V, E> path, E edge) {
        V edgeSource = path.getGraph().getEdgeSource(edge);
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

    public List<Pair<V, TranspilerNode>> run() {
        Stream<Pair<V, TranspilerNode>> reachingConditionAllPaths = slice.allPaths().stream().flatMap(path -> IntStream.range(0, path.getVertexList().size()).mapToObj(i -> (Pair<V, List<V>>) ImmutablePair.of(path.getVertexList().get(i), path.getVertexList().subList(0, i))).toList().stream().map(rc -> (Pair<V, TranspilerNode>) ImmutablePair.of(rc.getLeft(), rc.getRight().stream()
                .filter(instr -> instr.sentinel() == BODY && instr.ref() instanceof IfTranspilerNode)
                .map(TranspilerInstruction::ref)
                .reduce(new PrimitiveValueTranspilerNode(TypedRecord.TRUE),
                        AndTranspilerNode::new))));
        List<Pair<V, TranspilerNode>> exhaustiveReachingConditions = reachingConditionAllPaths
                .collect(Collectors.groupingBy(Pair::getLeft)).entrySet().stream()
                .map(p -> ImmutablePair.of(p.getKey(), p.getValue().stream()
                        .map(Pair::getRight)))
                .map(p1 -> (Pair<V, TranspilerNode>) ImmutablePair.of(p1.getLeft(), p1.getRight()
                        .reduce(new PrimitiveValueTranspilerNode(TypedRecord.TRUE), OrTranspilerNode::new))).toList();
        return exhaustiveReachingConditions;
//        return slice.allPaths().stream().map(path -> path.getVertexList().stream().filter(v -> v.sentinel() == BODY && v.ref() instanceof IfTranspilerNode)
//                .map(obj -> ((IfTranspilerNode) obj.ref()).getCondition())
//                .reduce(new PrimitiveValueTranspilerNode(TypedRecord.TRUE), AndTranspilerNode::new)).collect(Collectors.toUnmodifiableSet());
    }
}
