package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.task.DepthFirstSearchOrderingTask;
import com.mojo.algorithms.domain.DepthFirstSpanningTree;
import com.mojo.algorithms.domain.TestNode;
import com.mojo.algorithms.domain.exception.CyclicGraphException;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class TopologicalSortTaskTest {
    @Test
    public void canSortSimpleAcyclicTopologically() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode v1 = node("1");
        TestNode v2 = node("2");
        TestNode v3 = node("3");

        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addVertex(v3);

        graph.addEdge(v1, v2);
        graph.addEdge(v1, v3);

        DepthFirstSearchOrderingTask<TestNode, DefaultEdge> dfsTask = new DepthFirstSearchOrderingTask<>(v1, graph, DefaultEdge.class);
        DepthFirstSpanningTree<TestNode, DefaultEdge> spanningTree = dfsTask.run();
        List<TestNode> testNodes = spanningTree.topologicallyOrdered();
        assertTrue(ImmutableList.of(v1, v2, v3).equals(testNodes)
        || ImmutableList.of(v1, v3, v2).equals(testNodes));
    }

    @Test()
    public void cannotSortCyclicTopologically() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode v1 = node("1");
        TestNode v2 = node("2");
        TestNode v3 = node("3");

        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addVertex(v3);
        graph.addVertex(v1);

        graph.addEdge(v1, v2);
        graph.addEdge(v1, v3);
        graph.addEdge(v3, v1);

        DepthFirstSearchOrderingTask<TestNode, DefaultEdge> dfsTask = new DepthFirstSearchOrderingTask<>(v1, graph, DefaultEdge.class);
        DepthFirstSpanningTree<TestNode, DefaultEdge> spanningTree = dfsTask.run();
        assertThrows(CyclicGraphException.class, spanningTree::topologicallyOrdered);
    }

    private TestNode node(String id) {
        return new TestNode(id);
    }
}

