package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableSet;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.id.Identifiable;
import org.smojol.toolkit.analysis.task.transpiler.LoopBodyDetectionTask;

import java.util.Collection;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class LoopBodyDetectionTaskTest {
    @Test
    public void canDetectLoopBodies() {
        Graph<LoopDetectionTestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        LoopDetectionTestNode v0 = node("0");
        LoopDetectionTestNode v6 = node("6");
        LoopDetectionTestNode v1 = node("1");
        LoopDetectionTestNode v2 = node("2");
        LoopDetectionTestNode v7 = node("7");
        LoopDetectionTestNode v3 = node("3");
        LoopDetectionTestNode v8 = node("8");
        LoopDetectionTestNode v9 = node("9");
        LoopDetectionTestNode v4 = node("4");
        LoopDetectionTestNode v5 = node("5");

        graph.addVertex(v0);
        graph.addVertex(v6);
        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addVertex(v7);
        graph.addVertex(v3);
        graph.addVertex(v8);
        graph.addVertex(v9);
        graph.addVertex(v4);
        graph.addVertex(v5);

        graph.addEdge(v0, v1);
        graph.addEdge(v0, v6);
        graph.addEdge(v1, v2);
        graph.addEdge(v1, v7);
        graph.addEdge(v2, v3);
        graph.addEdge(v7, v3);
        graph.addEdge(v7, v8);
        graph.addEdge(v8, v9);
        graph.addEdge(v3, v4);
        graph.addEdge(v3, v9);
        graph.addEdge(v4, v3);
        graph.addEdge(v4, v5);
        graph.addEdge(v9, v5);
        graph.addEdge(v5, v1);
        graph.addEdge(v5, v7);
        graph.addEdge(v5, v6);

        LoopBodyDetectionTask<LoopDetectionTestNode, DefaultEdge> task = new LoopBodyDetectionTask<>(v0, graph, DefaultEdge.class);
        Pair<Set<Set<LoopDetectionTestNode>>, Set<Set<LoopDetectionTestNode>>> loopBodies = task.run();
        Set<Set<LoopDetectionTestNode>> reducibleLoopBodies = loopBodies.getLeft();
        Set<Set<LoopDetectionTestNode>> irreducibleLoopBodies = loopBodies.getRight();
        assertEquals(2, reducibleLoopBodies.size());
        assertEquals(1, irreducibleLoopBodies.size());
        assertTrue(reducibleLoopBodies.contains(ImmutableSet.of(n("1"), n("2"), n("3")))
                || reducibleLoopBodies.contains(ImmutableSet.of(n("1"), n("2"), n("5"))));
        assertTrue(reducibleLoopBodies.contains(ImmutableSet.of(n("3"), n("4"))));
        assertEquals(ImmutableSet.of(n("3"), n("5"), n("7"), n("8"), n("9")), irreducibleLoopBodies.stream().flatMap(Collection::stream).collect(Collectors.toUnmodifiableSet()));

        reducibleLoopBodies.forEach(rlb -> System.out.println(String.join(",", rlb.stream().map(LoopDetectionTestNode::id).toList())));
    }

    private static LoopDetectionTestNode n(String id) {
        return new LoopDetectionTestNode(id);
    }

    private static LoopDetectionTestNode node(String id) {
        return n(id);
    }
}


record LoopDetectionTestNode(String id) implements Identifiable {
    @Override
    public String label() {
        return id;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LoopDetectionTestNode that)) return false;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
