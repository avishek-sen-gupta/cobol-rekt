package org.poc.common;

import com.google.common.collect.ImmutableList;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.graph.ClassifiedEdges;
import org.smojol.common.graph.DepthFirstSpanningTree;
import org.smojol.common.graph.DepthFirstTraversalLabelTask;
import org.smojol.common.id.Identifiable;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class DepthFirstTraversalLabelTaskTest {
    @Test
    public void canLabelDFSOrderForSimpleGraphWithNoLoops() {
        Graph<DFSTestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        DFSTestNode va = new DFSTestNode("A");
        DFSTestNode vb = new DFSTestNode("B");
        DFSTestNode vc = new DFSTestNode("C");
        graph.addVertex(va);
        graph.addVertex(vb);
        graph.addVertex(vc);
        graph.addEdge(va, vb);
        graph.addEdge(va, vc);
        DepthFirstTraversalLabelTask<DFSTestNode, DefaultEdge> task = new DepthFirstTraversalLabelTask<>(va, graph, DefaultEdge.class);
        DepthFirstSpanningTree<DFSTestNode, DefaultEdge> spanningTree = task.run();
        List<DFSTestNode> ordered = spanningTree.preOrder();
        assertEquals(va, ordered.get(0));
        assertEquals(vb, ordered.get(1));
        assertEquals(vc, ordered.get(2));
        assertEquals(6, task.currentClock());
        assertTrue(spanningTree.isAncestorOf(va, vb));
        assertTrue(spanningTree.isAncestorOf(va, vc));
        assertFalse(spanningTree.isAncestorOf(vc, vb));
    }

    @Test
    public void canLabelDFSOrderForSimpleGraphWithBranches() {
    /*
    0 -> 1 -> 2 -> 3 -> 4 -> 5
         |    |    ↑         ↑
         |____|____|         |
              └---> 6 --> 7 -┘
     */
        Graph<DFSTestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        DFSTestNode v0 = new DFSTestNode("0");
        DFSTestNode v1 = new DFSTestNode("1");
        DFSTestNode v2 = new DFSTestNode("2");
        DFSTestNode v3 = new DFSTestNode("3");
        DFSTestNode v4 = new DFSTestNode("4");
        DFSTestNode v5 = new DFSTestNode("5");
        DFSTestNode v6 = new DFSTestNode("6");
        DFSTestNode v7 = new DFSTestNode("7");

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

        DepthFirstTraversalLabelTask<DFSTestNode, DefaultEdge> task = new DepthFirstTraversalLabelTask<>(v0, graph, DefaultEdge.class);
        DepthFirstSpanningTree<DFSTestNode, DefaultEdge> spanningTree = task.run();
        List<DFSTestNode> ordered = spanningTree.preOrder();
        assertEquals(v0, ordered.get(0));
        assertEquals(v1, ordered.get(1));
        assertEquals(v2, ordered.get(2));
        assertEquals(v3, ordered.get(3));
        assertEquals(v4, ordered.get(4));
        assertEquals(v5, ordered.get(5));
        assertEquals(v6, ordered.get(6));
        assertEquals(v7, ordered.get(7));
        assertEquals(16, task.currentClock());
        assertFalse(spanningTree.isAncestorOf(v3, v6));
        assertFalse(spanningTree.isAncestorOf(v6, v3));
        assertFalse(spanningTree.isAncestorOf(v4, v7));
        assertFalse(spanningTree.isAncestorOf(v7, v4));
        assertTrue(spanningTree.isAncestorOf(v2, v5));
    }

    /*
                 +---------+
                 v         |
     +---+     +---+     +---+     +---+     +---+
     | 1 | --> | 2 | --> | 3 | --> | 4 | --> | 5 |
     +---+     +---+     +---+     +---+     +---+
       |                   |         ^         ^
       |                   +---------+---------+
       v                             |
     +---+                           |
  +- | 6 | --------------------------+
  |  +---+
  |
  |    +---------+
  |    v         |
  |  +---+     +---+
  +> | 7 | --> | 8 |
     +---+     +---+
  */
    @Test
    public void canLabelDFSOrderForSimpleGraphWithBackEdges() {
        Graph<DFSTestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        DFSTestNode v1 = new DFSTestNode("1");
        DFSTestNode v2 = new DFSTestNode("2");
        DFSTestNode v3 = new DFSTestNode("3");
        DFSTestNode v4 = new DFSTestNode("4");
        DFSTestNode v5 = new DFSTestNode("5");
        DFSTestNode v6 = new DFSTestNode("6");
        DFSTestNode v7 = new DFSTestNode("7");
        DFSTestNode v8 = new DFSTestNode("8");

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

        DepthFirstTraversalLabelTask<DFSTestNode, DefaultEdge> task = new DepthFirstTraversalLabelTask<>(v1, graph, 1, DefaultEdge.class);
        DepthFirstSpanningTree<DFSTestNode, DefaultEdge> spanningTree = task.run();
        assertEquals(17, task.currentClock());
        assertEquals(ImmutableList.of(v1, v2, v3, v4, v5, v6, v7, v8), spanningTree.preOrder());
        assertEquals(ImmutableList.of(v8, v7, v6, v5, v4, v3, v2, v1), spanningTree.postOrder());
    }

    @Test
    public void canClassifyEdges() {
        Graph<DFSTestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        DFSTestNode vS = new DFSTestNode("S");
        DFSTestNode vA = new DFSTestNode("A");
        DFSTestNode vB = new DFSTestNode("B");
        DFSTestNode vC = new DFSTestNode("C");
        DFSTestNode vD = new DFSTestNode("D");
        DFSTestNode vE = new DFSTestNode("E");
        DFSTestNode vF = new DFSTestNode("F");
        DFSTestNode vG = new DFSTestNode("G");
        DFSTestNode vH = new DFSTestNode("H");
        DFSTestNode vI = new DFSTestNode("I");
        DFSTestNode vJ = new DFSTestNode("J");
        DFSTestNode vK = new DFSTestNode("K");

        graph.addVertex(vS);
        graph.addVertex(vA);
        graph.addVertex(vB);
        graph.addVertex(vC);
        graph.addVertex(vD);
        graph.addVertex(vE);
        graph.addVertex(vF);
        graph.addVertex(vG);
        graph.addVertex(vH);
        graph.addVertex(vI);
        graph.addVertex(vJ);
        graph.addVertex(vK);

        graph.addEdge(vS, vB);
        graph.addEdge(vS, vC);
        graph.addEdge(vB, vA);
        graph.addEdge(vB, vD);
        graph.addEdge(vB, vE);
        graph.addEdge(vA, vD);
        graph.addEdge(vD, vH);
        graph.addEdge(vE, vH);
        graph.addEdge(vH, vK);
        graph.addEdge(vH, vB);
        graph.addEdge(vK, vS);

        graph.addEdge(vC, vF);
        graph.addEdge(vC, vG);
        graph.addEdge(vF, vI);
        graph.addEdge(vG, vI);
        graph.addEdge(vG, vJ);
        graph.addEdge(vJ, vI);
        graph.addEdge(vI, vK);
        graph.addEdge(vI, vS);

        DepthFirstTraversalLabelTask<DFSTestNode, DefaultEdge> task = new DepthFirstTraversalLabelTask<>(vS, graph, 1, DefaultEdge.class);
        DepthFirstSpanningTree<DFSTestNode, DefaultEdge> spanningTree = task.run();
        ClassifiedEdges<DefaultEdge> classifiedEdges = spanningTree.classifiedEdges();
        assertEquals(11, classifiedEdges.treeEdges().size());
        assertEquals(3, classifiedEdges.backEdges().size());
        assertEquals(1, classifiedEdges.forwardEdges().size());
        assertEquals(4, classifiedEdges.crossEdges().size());
    }
}

record DFSTestNode(String id) implements Identifiable {
    @Override
    public String label() {
        return id;
    }
}
