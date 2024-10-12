package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.graph.DepthFirstSearchOrderingTask;
import org.smojol.common.graph.DepthFirstSpanningTree;
import org.smojol.common.graph.exception.CyclicGraphException;
import org.smojol.common.id.Identifiable;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class TopologicalSortTaskTest {
    @Test
    public void canSortSimpleAcyclicTopologically() {
        Graph<ToplogicalSortTestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        ToplogicalSortTestNode v1 = node("1");
        ToplogicalSortTestNode v2 = node("2");
        ToplogicalSortTestNode v3 = node("3");

        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addVertex(v3);

        graph.addEdge(v1, v2);
        graph.addEdge(v1, v3);

        DepthFirstSearchOrderingTask<ToplogicalSortTestNode, DefaultEdge> dfsTask = new DepthFirstSearchOrderingTask<>(v1, graph, DefaultEdge.class);
        DepthFirstSpanningTree<ToplogicalSortTestNode, DefaultEdge> spanningTree = dfsTask.run();
        List<ToplogicalSortTestNode> toplogicalSortTestNodes = spanningTree.topologicallyOrdered();
        assertTrue(ImmutableList.of(v1, v2, v3).equals(toplogicalSortTestNodes)
        || ImmutableList.of(v1, v3, v2).equals(toplogicalSortTestNodes));
    }

    @Test()
    public void cannotSortCyclicTopologically() {
        Graph<ToplogicalSortTestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        ToplogicalSortTestNode v1 = node("1");
        ToplogicalSortTestNode v2 = node("2");
        ToplogicalSortTestNode v3 = node("3");

        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addVertex(v3);
        graph.addVertex(v1);

        graph.addEdge(v1, v2);
        graph.addEdge(v1, v3);
        graph.addEdge(v3, v1);

        DepthFirstSearchOrderingTask<ToplogicalSortTestNode, DefaultEdge> dfsTask = new DepthFirstSearchOrderingTask<>(v1, graph, DefaultEdge.class);
        DepthFirstSpanningTree<ToplogicalSortTestNode, DefaultEdge> spanningTree = dfsTask.run();
        assertThrows(CyclicGraphException.class, spanningTree::topologicallyOrdered);
    }

    private ToplogicalSortTestNode node(String id) {
        return new ToplogicalSortTestNode(id);
    }
}

class ToplogicalSortTestNode implements Identifiable {
    private final String id;

    public ToplogicalSortTestNode(String id) {
        this.id = id;
    }

    @Override
    public String id() {
        return id;
    }

    @Override
    public String label() {
        return id;
    }
}
