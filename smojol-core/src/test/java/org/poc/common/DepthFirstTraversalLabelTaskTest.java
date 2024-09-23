package org.poc.common;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.graph.CodeGraphNode;
import org.smojol.common.graph.DepthFirstTraversalLabelTask;
import org.smojol.common.graph.GraphNodeLike;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.smojol.common.graph.DepthFirstTraversalLabelTask.DFS_NUM;

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
        assertEquals(0, va.getProperty(DFS_NUM, Integer.class));
        assertEquals(1, vb.getProperty(DFS_NUM, Integer.class));
        assertEquals(2, vc.getProperty(DFS_NUM, Integer.class));
        assertEquals(3, task.max());
    }

    @Test
    public void canLabelDFSOrderForSimpleGraphWithBranches() {
    /*
    0 -> 1 -> 2 -> 3 -> 4 -> 5
         |    |    ↑         ↑
         |____|____|         |
              └---> 6 --> 7 -┘
     */
        Graph<GraphNodeLike, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        CodeGraphNode v0 = new CodeGraphNode("0");
        CodeGraphNode v1 = new CodeGraphNode("1");
        CodeGraphNode v2 = new CodeGraphNode("2");
        CodeGraphNode v3 = new CodeGraphNode("3");
        CodeGraphNode v4 = new CodeGraphNode("4");
        CodeGraphNode v5 = new CodeGraphNode("5");
        CodeGraphNode v6 = new CodeGraphNode("6");
        CodeGraphNode v7 = new CodeGraphNode("7");

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

        DepthFirstTraversalLabelTask task = new DepthFirstTraversalLabelTask(v0, graph);
        task.run();
        assertEquals(0, v0.getProperty(DFS_NUM, Integer.class));
        assertEquals(1, v1.getProperty(DFS_NUM, Integer.class));
        assertEquals(2, v2.getProperty(DFS_NUM, Integer.class));
        assertEquals(3, v3.getProperty(DFS_NUM, Integer.class));
        assertEquals(4, v4.getProperty(DFS_NUM, Integer.class));
        assertEquals(5, v5.getProperty(DFS_NUM, Integer.class));
        assertEquals(6, v6.getProperty(DFS_NUM, Integer.class));
        assertEquals(7, v7.getProperty(DFS_NUM, Integer.class));
        assertEquals(8, task.max());
    }

    /*
     1
    / \
   2   7
  ||   |
   3   |
  ||  / \
 /  \/   8
 |   4   |
 |   |   8
 ----6
     */
    @Test
    public void canLabelDFSOrderForSimpleGraphWithBackEdges() {
        Graph<GraphNodeLike, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        CodeGraphNode v1 = new CodeGraphNode("1");
        CodeGraphNode v2 = new CodeGraphNode("2");
        CodeGraphNode v3 = new CodeGraphNode("3");
        CodeGraphNode v4 = new CodeGraphNode("4");
        CodeGraphNode v5 = new CodeGraphNode("5");
        CodeGraphNode v6 = new CodeGraphNode("6");
        CodeGraphNode v7 = new CodeGraphNode("7");
        CodeGraphNode v8 = new CodeGraphNode("8");

        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addVertex(v3);
        graph.addVertex(v4);
        graph.addVertex(v5);
        graph.addVertex(v6);
        graph.addVertex(v7);
        graph.addVertex(v8);

        graph.addEdge(v1, v2);
        graph.addEdge(v1, v6);
        graph.addEdge(v2, v3);
        graph.addEdge(v3, v2);
        graph.addEdge(v6, v4);
        graph.addEdge(v6, v7);
        graph.addEdge(v3, v4);
        graph.addEdge(v3, v5);
        graph.addEdge(v4, v5);
        graph.addEdge(v7, v8);
        graph.addEdge(v8, v7);

        DepthFirstTraversalLabelTask task = new DepthFirstTraversalLabelTask(v1, graph, 1);
        task.run();
        assertEquals(1, v1.getProperty(DFS_NUM, Integer.class));
        assertEquals(2, v2.getProperty(DFS_NUM, Integer.class));
        assertEquals(3, v3.getProperty(DFS_NUM, Integer.class));
        assertEquals(4, v4.getProperty(DFS_NUM, Integer.class));
        assertEquals(5, v5.getProperty(DFS_NUM, Integer.class));
        assertEquals(6, v6.getProperty(DFS_NUM, Integer.class));
        assertEquals(7, v7.getProperty(DFS_NUM, Integer.class));
        assertEquals(8, v8.getProperty(DFS_NUM, Integer.class));
        assertEquals(9, task.max());
        assertEquals(ImmutableList.of(v1, v2, v3, v4, v5, v6, v7, v8), task.preOrder());
        assertEquals(ImmutableList.of(v8, v7, v6, v5, v4, v3, v2, v1), task.postOrder());
    }

    @Test
    public void canFindDominatorSets() {
    /*
    0 -> 1 -> 2 -> 3 -> 4 -> 5
         |    |    ↑         ↑
         |____|____|         |
              └---> 6 --> 7 -┘
     */
        Graph<GraphNodeLike, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        CodeGraphNode v0 = new CodeGraphNode("0");
        CodeGraphNode v1 = new CodeGraphNode("1");
        CodeGraphNode v2 = new CodeGraphNode("2");
        CodeGraphNode v3 = new CodeGraphNode("3");
        CodeGraphNode v4 = new CodeGraphNode("4");
        CodeGraphNode v5 = new CodeGraphNode("5");
        CodeGraphNode v6 = new CodeGraphNode("6");
        CodeGraphNode v7 = new CodeGraphNode("7");

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

        DepthFirstTraversalLabelTask task = new DepthFirstTraversalLabelTask(v0, graph);

        Map<GraphNodeLike, Set<GraphNodeLike>> dominatorSets = task.allDominators();
        assertEquals(ImmutableSet.of(v0), dominatorSets.get(v0));
        assertEquals(ImmutableSet.of(v0, v1), dominatorSets.get(v1));
        assertEquals(ImmutableSet.of(v0, v1, v2), dominatorSets.get(v2));
        assertEquals(ImmutableSet.of(v0, v1, v3), dominatorSets.get(v3));
        assertEquals(ImmutableSet.of(v0, v1, v3, v4), dominatorSets.get(v4));
        assertEquals(ImmutableSet.of(v0, v1, v5), dominatorSets.get(v5));
        assertEquals(ImmutableSet.of(v0, v1, v2, v6), dominatorSets.get(v6));
        assertEquals(ImmutableSet.of(v0, v1, v2, v6, v7), dominatorSets.get(v7));
    }

    @Test
    public void canFindImmediateDominators() {
    /*
    0 -> 1 -> 2 -> 3 -> 4 -> 5
         |    |    ↑         ↑
         |____|____|         |
              └---> 6 --> 7 -┘
     */
        Graph<GraphNodeLike, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        CodeGraphNode v0 = new CodeGraphNode("0");
        CodeGraphNode v1 = new CodeGraphNode("1");
        CodeGraphNode v2 = new CodeGraphNode("2");
        CodeGraphNode v3 = new CodeGraphNode("3");
        CodeGraphNode v4 = new CodeGraphNode("4");
        CodeGraphNode v5 = new CodeGraphNode("5");
        CodeGraphNode v6 = new CodeGraphNode("6");
        CodeGraphNode v7 = new CodeGraphNode("7");

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

        DepthFirstTraversalLabelTask task = new DepthFirstTraversalLabelTask(v0, graph);

        List<ImmutablePair<GraphNodeLike, GraphNodeLike>> immediateDominators = task.immediateDominators();
        assertTrue(immediateDominators.contains(ImmutablePair.of(v0, v0)));
        assertTrue(immediateDominators.contains(ImmutablePair.of(v1, v0)));
        assertTrue(immediateDominators.contains(ImmutablePair.of(v2, v1)));
        assertTrue(immediateDominators.contains(ImmutablePair.of(v3, v1)));
        assertTrue(immediateDominators.contains(ImmutablePair.of(v4, v3)));
        assertTrue(immediateDominators.contains(ImmutablePair.of(v5, v1)));
        assertTrue(immediateDominators.contains(ImmutablePair.of(v6, v2)));
        assertTrue(immediateDominators.contains(ImmutablePair.of(v7, v6)));
    }
}
