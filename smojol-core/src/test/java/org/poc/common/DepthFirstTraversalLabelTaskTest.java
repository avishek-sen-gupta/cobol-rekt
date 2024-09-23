package org.poc.common;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.graph.CodeGraphNode;
import org.smojol.common.graph.DepthFirstTraversalLabelTask;
import org.smojol.common.graph.GraphNodeLike;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class DepthFirstTraversalLabelTaskTest {
    @Test
    public void canLabelDFSOrderForSimpleGraphWithNoLoops() {
        Graph<GraphNodeLike, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        CodeGraphNode va = new CodeGraphNode("A");
        CodeGraphNode vb = new CodeGraphNode("B");
        CodeGraphNode vc = new CodeGraphNode("C");
        graph.addVertex(va);
        graph.addVertex(vb);
        graph.addVertex(vc);
        graph.addEdge(va, vb);
        graph.addEdge(va, vc);
        DepthFirstTraversalLabelTask task = new DepthFirstTraversalLabelTask(va, graph);
        task.run();
        assertEquals(0, va.getProperty("DFS_NUM", Integer.class));
        assertEquals(1, vb.getProperty("DFS_NUM", Integer.class));
        assertEquals(2, vc.getProperty("DFS_NUM", Integer.class));
        assertEquals(2, task.max());
    }

    @Test
    public void canLabelDFSOrderForSimpleGraphWithBranches() {
        Graph<GraphNodeLike, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        CodeGraphNode v0 = new CodeGraphNode("A");
        CodeGraphNode v1 = new CodeGraphNode("B");
        CodeGraphNode v2 = new CodeGraphNode("C");
        CodeGraphNode v3 = new CodeGraphNode("D");
        CodeGraphNode v4 = new CodeGraphNode("E");
        CodeGraphNode v5 = new CodeGraphNode("F");
        CodeGraphNode v6 = new CodeGraphNode("G");
        CodeGraphNode v7 = new CodeGraphNode("H");

        graph.addVertex(v0);
        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addVertex(v3);
        graph.addVertex(v4);
        graph.addVertex(v5);
        graph.addVertex(v6);
        graph.addVertex(v7);
        graph.addEdge(v0, v1);
        graph.addEdge(v0, v2);
        graph.addEdge(v2, v3);
        graph.addEdge(v3, v4);
        graph.addEdge(v4, v5);

        // DETOUR 1
        graph.addEdge(v1, v3);

        // DETOUR 2
        graph.addEdge(v2, v6);
        graph.addEdge(v6, v7);
        graph.addEdge(v7, v5);

        DepthFirstTraversalLabelTask task = new DepthFirstTraversalLabelTask(v0, graph);
        task.run();
        assertEquals(0, v0.getProperty("DFS_NUM", Integer.class));
        assertEquals(1, v1.getProperty("DFS_NUM", Integer.class));
        assertEquals(5, v2.getProperty("DFS_NUM", Integer.class));
        assertEquals(2, v3.getProperty("DFS_NUM", Integer.class));
        assertEquals(3, v4.getProperty("DFS_NUM", Integer.class));
        assertEquals(4, v5.getProperty("DFS_NUM", Integer.class));
        assertEquals(6, v6.getProperty("DFS_NUM", Integer.class));
        assertEquals(7, v7.getProperty("DFS_NUM", Integer.class));
        assertEquals(8, task.max());
    }
}
