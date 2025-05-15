package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableSet;
import com.mojo.algorithms.task.DepthFirstSearchOrderingTask;
import com.mojo.algorithms.domain.DepthFirstSpanningTree;
import com.mojo.algorithms.domain.TestNode;
import com.mojo.algorithms.task.BuildDominatorsTask;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

public class BuildDominatorsTaskTest {
    @Test
    public void canFindDominatorSets() {
    /*
    0 -> 1 -> 2 -> 3 -> 4 -> 5
         |    |    ↑         ↑
         |____|____|         |
              └---> 6 --> 7 -┘
     */
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode v0 = getCodeGraphNode("0");
        TestNode v1 = getCodeGraphNode("1");
        TestNode v2 = getCodeGraphNode("2");
        TestNode v3 = getCodeGraphNode("3");
        TestNode v4 = getCodeGraphNode("4");
        TestNode v5 = getCodeGraphNode("5");
        TestNode v6 = getCodeGraphNode("6");
        TestNode v7 = getCodeGraphNode("7");

        graph.addVertex(v0);
        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addVertex(v3);
        graph.addVertex(v4);
        graph.addVertex(v5);
        graph.addVertex(v6);
        graph.addVertex(v7);
        graph.addEdge(v0, v1);
        graph.addEdge(v1, v2);
        graph.addEdge(v2, v3);
        graph.addEdge(v3, v4);
        graph.addEdge(v4, v5);

        // DETOUR 1
        graph.addEdge(v1, v3);

        // DETOUR 2
        graph.addEdge(v2, v6);
        graph.addEdge(v6, v7);
        graph.addEdge(v7, v5);

        DepthFirstSearchOrderingTask<TestNode, DefaultEdge> dfsTask = new DepthFirstSearchOrderingTask<>(v0, graph, DefaultEdge.class);
        DepthFirstSpanningTree<TestNode, DefaultEdge> spanningTree = dfsTask.run();

        Map<TestNode, Set<TestNode>> dominatorSets = new BuildDominatorsTask<TestNode, DefaultEdge>().allDominators(spanningTree.preOrdered(), graph);
        assertEquals(ImmutableSet.of(v0), dominatorSets.get(v0));
        assertEquals(ImmutableSet.of(v0, v1), dominatorSets.get(v1));
        assertEquals(ImmutableSet.of(v0, v1, v2), dominatorSets.get(v2));
        assertEquals(ImmutableSet.of(v0, v1, v3), dominatorSets.get(v3));
        assertEquals(ImmutableSet.of(v0, v1, v3, v4), dominatorSets.get(v4));
        assertEquals(ImmutableSet.of(v0, v1, v5), dominatorSets.get(v5));
        assertEquals(ImmutableSet.of(v0, v1, v2, v6), dominatorSets.get(v6));
        assertEquals(ImmutableSet.of(v0, v1, v2, v6, v7), dominatorSets.get(v7));
    }

    private static TestNode getCodeGraphNode(String id) {
        return new TestNode(id);
    }

    @Test
    public void canFindImmediateDominators() {
    /*
    0 -> 1 -> 2 -> 3 -> 4 -> 5
         |    |    ↑         ↑
         |____|____|         |
              └---> 6 --> 7 -┘
     */
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode v0 = getCodeGraphNode("0");
        TestNode v1 = getCodeGraphNode("1");
        TestNode v2 = getCodeGraphNode("2");
        TestNode v3 = getCodeGraphNode("3");
        TestNode v4 = getCodeGraphNode("4");
        TestNode v5 = getCodeGraphNode("5");
        TestNode v6 = getCodeGraphNode("6");
        TestNode v7 = getCodeGraphNode("7");

        graph.addVertex(v0);
        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addVertex(v3);
        graph.addVertex(v4);
        graph.addVertex(v5);
        graph.addVertex(v6);
        graph.addVertex(v7);
        graph.addEdge(v0, v1);
        graph.addEdge(v1, v2);
        graph.addEdge(v2, v3);
        graph.addEdge(v3, v4);
        graph.addEdge(v4, v5);

        // DETOUR 1
        graph.addEdge(v1, v3);

        // DETOUR 2
        graph.addEdge(v2, v6);
        graph.addEdge(v6, v7);
        graph.addEdge(v7, v5);

        DepthFirstSearchOrderingTask<TestNode, DefaultEdge> dfsTask = new DepthFirstSearchOrderingTask<>(v0, graph, DefaultEdge.class);
        DepthFirstSpanningTree<TestNode, DefaultEdge> spanningTree = dfsTask.run();

        List<Pair<TestNode, TestNode>> immediateDominators = new BuildDominatorsTask<TestNode, DefaultEdge>().immediateDominators(spanningTree);
        assertFalse(immediateDominators.contains(ImmutablePair.of(v0, v0)));
        assertTrue(immediateDominators.contains(ImmutablePair.of(v0, null)));
        assertTrue(immediateDominators.contains(ImmutablePair.of(v1, v0)));
        assertTrue(immediateDominators.contains(ImmutablePair.of(v2, v1)));
        assertTrue(immediateDominators.contains(ImmutablePair.of(v3, v1)));
        assertTrue(immediateDominators.contains(ImmutablePair.of(v4, v3)));
        assertTrue(immediateDominators.contains(ImmutablePair.of(v5, v1)));
        assertTrue(immediateDominators.contains(ImmutablePair.of(v6, v2)));
        assertTrue(immediateDominators.contains(ImmutablePair.of(v7, v6)));
    }
}
