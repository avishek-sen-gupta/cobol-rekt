package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableSet;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.analysis.NaturalLoopBody;
import org.smojol.common.graph.TestNode;
import org.smojol.toolkit.analysis.task.transpiler.CloneEdgeOperation;
import org.smojol.toolkit.analysis.task.transpiler.LoopBodyDetectionTask;

import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class LoopBodyDetectionTaskTest {
    @Test
    public void canDetectLoopBodies1() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode v0 = node("0");
        TestNode v6 = node("6");
        TestNode v1 = node("1");
        TestNode v2 = node("2");
        TestNode v7 = node("7");
        TestNode v3 = node("3");
        TestNode v8 = node("8");
        TestNode v9 = node("9");
        TestNode v4 = node("4");
        TestNode v5 = node("5");

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

        LoopBodyDetectionTask<TestNode, DefaultEdge> task = new LoopBodyDetectionTask<>(v0, graph, DefaultEdge.class, CloneEdgeOperation::cloneEdge);
        Pair<Set<NaturalLoopBody<TestNode>>, Set<NaturalLoopBody<TestNode>>> loopBodies = task.run();
        Set<Set<TestNode>> justReducibleLoopBodies = loopBodies.getLeft().stream().map(NaturalLoopBody::loopNodes).collect(Collectors.toUnmodifiableSet());
        Set<Set<TestNode>> irreducibleLoopBodies = loopBodies.getRight().stream().map(NaturalLoopBody::loopNodes).collect(Collectors.toUnmodifiableSet());
        assertEquals(2, loopBodies.getLeft().size());
        assertEquals(1, irreducibleLoopBodies.size());
        assertTrue(justReducibleLoopBodies.contains(ImmutableSet.of(n("1"), n("2"), n("3")))
                || justReducibleLoopBodies.contains(ImmutableSet.of(n("1"), n("2"), n("5"))));
        assertTrue(justReducibleLoopBodies.contains(ImmutableSet.of(n("3"), n("4"))));
        assertEquals(ImmutableSet.of(n("3"), n("5"), n("7"), n("8"), n("9")), irreducibleLoopBodies.stream().flatMap(Collection::stream).collect(Collectors.toUnmodifiableSet()));

        loopBodies.getLeft().forEach(rlb -> System.out.println(String.join(",", rlb.loopNodes().stream().map(TestNode::id).toList())));
    }
    @Test
    public void canDetectLoopBodies2() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode v0 = node("0");
        TestNode v1 = node("1");
        TestNode v2 = node("2");

        graph.addVertex(v0);
        graph.addVertex(v1);
        graph.addVertex(v2);

        graph.addEdge(v0, v1);
        graph.addEdge(v0, v2);
        graph.addEdge(v1, v2);
        graph.addEdge(v2, v1);

        LoopBodyDetectionTask<TestNode, DefaultEdge> task = new LoopBodyDetectionTask<>(v0, graph, DefaultEdge.class, CloneEdgeOperation::cloneEdge);
        Pair<Set<NaturalLoopBody<TestNode>>, Set<NaturalLoopBody<TestNode>>> loopBodies = task.run();
        Set<NaturalLoopBody<TestNode>> reducibleLoopBodies = loopBodies.getLeft();
//        Set<Set<TestNode>> irreducibleLoopBodies = loopBodies.getRight().stream().map(NaturalLoopBody::loopNodes).collect(Collectors.toUnmodifiableSet());
        Set<Set<TestNode>> irreducibleLoopBodies = loopBodies.getRight().stream().map(NaturalLoopBody::loopNodes).collect(Collectors.toUnmodifiableSet());
        assertEquals(0, reducibleLoopBodies.size());
        assertEquals(1, irreducibleLoopBodies.size());
        assertTrue(irreducibleLoopBodies.contains(ImmutableSet.of(n("1"), n("2"))));
        assertTrue(loopBodies.getRight().stream().findFirst().get().loopHeaders().size() > 1);
    }

    private static TestNode n(String id) {
        return new TestNode(id);
    }

    private static TestNode node(String id) {
        return n(id);
    }
}

