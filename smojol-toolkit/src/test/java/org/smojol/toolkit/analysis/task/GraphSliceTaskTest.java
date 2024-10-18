package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.graph.GraphSliceTask;
import org.smojol.common.graph.TestNode;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class GraphSliceTaskTest {
    @Test
    public void canFindSimplePathInAcyclicTree() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode v1 = node("1");
        TestNode v2 = node("2");
        TestNode v3 = node("3");
        TestNode v4 = node("4");

        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addVertex(v3);
        graph.addVertex(v4);

        graph.addEdge(v1, v2);
        graph.addEdge(v2, v4);
        graph.addEdge(v1, v3);

        List<GraphPath<TestNode, DefaultEdge>> allPathsFromV1ToV3 = new GraphSliceTask<>(graph, DefaultEdge.class).run(v1, v3).allPaths();
        assertEquals(1, allPathsFromV1ToV3.size());
        assertEquals(ImmutableList.of(v1, v3), allPathsFromV1ToV3.getFirst().getVertexList());
        assertEquals(1, allPathsFromV1ToV3.getFirst().getEdgeList().size());
        DefaultEdge onlyEdge = allPathsFromV1ToV3.getFirst().getEdgeList().getFirst();
        assertEquals(v1, graph.getEdgeSource(onlyEdge));
        assertEquals(v3, graph.getEdgeTarget(onlyEdge));

        List<GraphPath<TestNode, DefaultEdge>> allPathsFromV1ToV4 = new GraphSliceTask<>(graph, DefaultEdge.class).run(v1, v4).allPaths();
        assertEquals(1, allPathsFromV1ToV4.size());
    }

    @Test
    public void canFindSimplePathInAcyclicGraph() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode v1 = node("1");
        TestNode v2 = node("2");
        TestNode v3 = node("3");
        TestNode v4 = node("4");

        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addVertex(v3);
        graph.addVertex(v4);

        graph.addEdge(v1, v2);
        graph.addEdge(v1, v3);
        graph.addEdge(v2, v4);
        graph.addEdge(v3, v4);

        List<GraphPath<TestNode, DefaultEdge>> allPathsFromV1ToV4 = new GraphSliceTask<>(graph, DefaultEdge.class).run(v1, v4).allPaths();
        assertEquals(2, allPathsFromV1ToV4.size());
        assertEquals(ImmutableList.of(v1, v2, v4), allPathsFromV1ToV4.getFirst().getVertexList());
        assertEquals(ImmutableList.of(v1, v3), allPathsFromV1ToV4.get(1).getVertexList());
    }

    @Test
    public void canFindTruncatedSimplePathInAcyclicGraph() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode v1 = node("1");
        TestNode v2 = node("2");
        TestNode v3 = node("3");
        TestNode v4 = node("4");
        TestNode v5 = node("5");

        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addVertex(v3);
        graph.addVertex(v4);
        graph.addVertex(v5);

        graph.addEdge(v1, v2);
        graph.addEdge(v1, v3);
        graph.addEdge(v2, v4);
        graph.addEdge(v3, v4);
        graph.addEdge(v4, v5);

        List<GraphPath<TestNode, DefaultEdge>> allPathsFromV1ToV5 = new GraphSliceTask<>(graph, DefaultEdge.class).run(v1, v5).allPaths();
        assertEquals(2, allPathsFromV1ToV5.size());
        assertEquals(ImmutableList.of(v1, v2, v4, v5), allPathsFromV1ToV5.getFirst().getVertexList());
        assertEquals(ImmutableList.of(v1, v3), allPathsFromV1ToV5.get(1).getVertexList());
    }

    private TestNode node(String id) {
        return new TestNode(id);
    }
}
