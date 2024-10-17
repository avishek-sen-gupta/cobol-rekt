package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.graph.GraphSlice;
import org.smojol.common.graph.GraphSliceTask;
import org.smojol.common.graph.ReachingConditionDefinitionTask;
import org.smojol.common.graph.TestNode;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ReachingConditionTaskTest {
    @Test
    public void canFindReachingConditionForSimpleAcyclicGraph() {
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

        GraphSlice<TestNode, DefaultEdge> graphSlice = new GraphSliceTask<>(graph).run(v1, v4);
        List<GraphPath<TestNode, DefaultEdge>> allPathsFromV1ToV4 = graphSlice.allPaths();
        assertEquals(2, allPathsFromV1ToV4.size());
        assertEquals(ImmutableList.of(v1, v2, v4), allPathsFromV1ToV4.getFirst().getVertexList());
        assertEquals(ImmutableList.of(v1, v3, v4), allPathsFromV1ToV4.get(1).getVertexList());
        new ReachingConditionDefinitionTask<>().run(null);
    }

    private TestNode node(String id) {
        return new TestNode(id);
    }
}
