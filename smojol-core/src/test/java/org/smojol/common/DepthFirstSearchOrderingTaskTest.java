package org.smojol.common;

import com.google.common.collect.ImmutableList;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.flowchart.MermaidGraph;
import org.smojol.common.graph.*;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

public class DepthFirstSearchOrderingTaskTest {
    @Test
    public void canLabelDFSOrderForSimpleGraphWithNoLoops() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode va = new TestNode("A");
        TestNode vb = new TestNode("B");
        TestNode vc = new TestNode("C");
        graph.addVertex(va);
        graph.addVertex(vb);
        graph.addVertex(vc);
        graph.addEdge(va, vb);
        graph.addEdge(va, vc);
        DepthFirstSearchOrderingTask<TestNode, DefaultEdge> task = new DepthFirstSearchOrderingTask<>(va, graph, DefaultEdge.class);
        DepthFirstSpanningTree<TestNode, DefaultEdge> spanningTree = task.run();
        List<TestNode> ordered = spanningTree.preOrdered();
        assertEquals(va, ordered.get(0));
        assertEquals(vb, ordered.get(1));
        assertEquals(vc, ordered.get(2));
        assertEquals(6, task.currentClock());
        assertTrue(spanningTree.isAncestorOf(va, vb));
        assertTrue(spanningTree.isAncestorOf(va, vc));
        assertFalse(spanningTree.isAncestorOf(vc, vb));

        assertEquals(0, spanningTree.treeDepth(va));
        assertEquals(1, spanningTree.treeDepth(vb));
        assertEquals(1, spanningTree.treeDepth(vc));
    }

    @Test
    public void canLabelTreeDepthForExampleDominatorTree() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode vSTART = new TestNode("START");
        TestNode vEND = new TestNode("END");
        TestNode vA = new TestNode("A");
        TestNode vB = new TestNode("B");
        TestNode vC = new TestNode("C");
        TestNode vD = new TestNode("D");
        TestNode vE = new TestNode("E");
        TestNode vF = new TestNode("F");
        TestNode vG = new TestNode("G");
        TestNode vH = new TestNode("H");
        graph.addVertex(vSTART);
        graph.addVertex(vA);
        graph.addVertex(vB);
        graph.addVertex(vC);
        graph.addVertex(vD);
        graph.addVertex(vE);
        graph.addVertex(vF);
        graph.addVertex(vG);
        graph.addVertex(vH);
        graph.addVertex(vEND);

        graph.addEdge(vSTART, vEND);
        graph.addEdge(vSTART, vA);
        graph.addEdge(vA, vB);
        graph.addEdge(vA, vC);
        graph.addEdge(vA, vD);
        graph.addEdge(vA, vF);
        graph.addEdge(vA, vH);
        graph.addEdge(vC, vE);
        graph.addEdge(vD, vG);

        DepthFirstSearchOrderingTask<TestNode, DefaultEdge> task = new DepthFirstSearchOrderingTask<>(vSTART, graph, DefaultEdge.class);
        DepthFirstSpanningTree<TestNode, DefaultEdge> spanningTree = task.run();

        assertEquals(0, spanningTree.treeDepth(vSTART));
        assertEquals(1, spanningTree.treeDepth(vA));
        assertEquals(1, spanningTree.treeDepth(vEND));
        assertEquals(2, spanningTree.treeDepth(vB));
        assertEquals(2, spanningTree.treeDepth(vC));
        assertEquals(2, spanningTree.treeDepth(vD));
        assertEquals(2, spanningTree.treeDepth(vF));
        assertEquals(2, spanningTree.treeDepth(vH));
        assertEquals(3, spanningTree.treeDepth(vE));
        assertEquals(3, spanningTree.treeDepth(vG));
    }

    @Test
    public void canLabelDFSOrderForSimpleGraphWithBranches() {
    /*
    0 -> 1 -> 2 -> 3 -> 4 -> 5
         |    |    ↑         ↑
         |____|____|         |
              └---> 6 --> 7 -┘
     */
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode v0 = new TestNode("0");
        TestNode v1 = new TestNode("1");
        TestNode v2 = new TestNode("2");
        TestNode v3 = new TestNode("3");
        TestNode v4 = new TestNode("4");
        TestNode v5 = new TestNode("5");
        TestNode v6 = new TestNode("6");
        TestNode v7 = new TestNode("7");

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

        DepthFirstSearchOrderingTask<TestNode, DefaultEdge> task = new DepthFirstSearchOrderingTask<>(v0, graph, DefaultEdge.class);
        DepthFirstSpanningTree<TestNode, DefaultEdge> spanningTree = task.run();
        List<TestNode> ordered = spanningTree.preOrdered();
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

        assertEquals(0, spanningTree.treeDepth(v0));
        assertEquals(1, spanningTree.treeDepth(v1));
        assertEquals(2, spanningTree.treeDepth(v2));
        assertEquals(3, spanningTree.treeDepth(v3));
        assertEquals(4, spanningTree.treeDepth(v4));
        assertEquals(5, spanningTree.treeDepth(v5));
        assertEquals(3, spanningTree.treeDepth(v6));
        assertEquals(4, spanningTree.treeDepth(v7));
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
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode v1 = new TestNode("1");
        TestNode v2 = new TestNode("2");
        TestNode v3 = new TestNode("3");
        TestNode v4 = new TestNode("4");
        TestNode v5 = new TestNode("5");
        TestNode v6 = new TestNode("6");
        TestNode v7 = new TestNode("7");
        TestNode v8 = new TestNode("8");

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

        DepthFirstSearchOrderingTask<TestNode, DefaultEdge> task = new DepthFirstSearchOrderingTask<>(v1, graph, 1, DefaultEdge.class);
        DepthFirstSpanningTree<TestNode, DefaultEdge> spanningTree = task.run();
        assertEquals(17, task.currentClock());
        assertEquals(ImmutableList.of(v1, v2, v3, v4, v5, v6, v7, v8), spanningTree.preOrdered());
        assertEquals(ImmutableList.of(v8, v7, v6, v5, v4, v3, v2, v1), spanningTree.postOrdered());


        assertEquals(0, spanningTree.treeDepth(v1));
        assertEquals(1, spanningTree.treeDepth(v2));
        assertEquals(2, spanningTree.treeDepth(v3));
        assertEquals(3, spanningTree.treeDepth(v4));
        assertEquals(4, spanningTree.treeDepth(v5));
        assertEquals(1, spanningTree.treeDepth(v6));
        assertEquals(2, spanningTree.treeDepth(v7));
        assertEquals(3, spanningTree.treeDepth(v8));
    }

    @Test
    public void canFindNaturalLoopOfBackEdge() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode v1 = new TestNode("1");
        TestNode v2 = new TestNode("2");
        TestNode v3 = new TestNode("3");
        TestNode v4 = new TestNode("4");
        TestNode v5 = new TestNode("5");
        TestNode v6 = new TestNode("6");
        TestNode v7 = new TestNode("7");
        TestNode v8 = new TestNode("8");

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

        DepthFirstSearchOrderingTask<TestNode, DefaultEdge> task = new DepthFirstSearchOrderingTask<>(v1, graph, 1, DefaultEdge.class);
        DepthFirstSpanningTree<TestNode, DefaultEdge> spanningTree = task.run();
        assertEquals(17, task.currentClock());
        assertEquals(ImmutableList.of(v1, v2, v3, v4, v5, v6, v7, v8), spanningTree.preOrdered());
        assertEquals(ImmutableList.of(v8, v7, v6, v5, v4, v3, v2, v1), spanningTree.postOrdered());


        assertEquals(0, spanningTree.treeDepth(v1));
        assertEquals(1, spanningTree.treeDepth(v2));
        assertEquals(2, spanningTree.treeDepth(v3));
        assertEquals(3, spanningTree.treeDepth(v4));
        assertEquals(4, spanningTree.treeDepth(v5));
        assertEquals(1, spanningTree.treeDepth(v6));
        assertEquals(2, spanningTree.treeDepth(v7));
        assertEquals(3, spanningTree.treeDepth(v8));

        DefaultEdge backEdge = graph.getEdge(v3, v2);
        Set<TestNode> loopNodes = new NaturalLoopOfBackEdgeTask<>(backEdge, graph).run();
        assertEquals(2, loopNodes.size());
        assertTrue(loopNodes.contains(v2));
        assertTrue(loopNodes.contains(v3));
    }

    @Test
    public void canClassifyEdges() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode vS = new TestNode("S");
        TestNode vA = new TestNode("A");
        TestNode vB = new TestNode("B");
        TestNode vC = new TestNode("C");
        TestNode vD = new TestNode("D");
        TestNode vE = new TestNode("E");
        TestNode vF = new TestNode("F");
        TestNode vG = new TestNode("G");
        TestNode vH = new TestNode("H");
        TestNode vI = new TestNode("I");
        TestNode vJ = new TestNode("J");
        TestNode vK = new TestNode("K");

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

        DepthFirstSearchOrderingTask<TestNode, DefaultEdge> task = new DepthFirstSearchOrderingTask<>(vS, graph, 1, DefaultEdge.class);
        DepthFirstSpanningTree<TestNode, DefaultEdge> spanningTree = task.run();
        ClassifiedEdges<DefaultEdge> classifiedEdges = spanningTree.classifiedEdges();
        assertEquals(11, classifiedEdges.treeEdges().size());
        assertEquals(3, classifiedEdges.backEdges().size());
        assertEquals(1, classifiedEdges.forwardEdges().size());
        assertEquals(4, classifiedEdges.crossEdges().size());
    }

    @Test
    public void doesNotLabelBackEdgesSpuriously() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode v0 = new TestNode("0");
        TestNode v6 = new TestNode("6");
        TestNode v1 = new TestNode("1");
        TestNode v2 = new TestNode("2");
        TestNode v7 = new TestNode("7");
        TestNode v3 = new TestNode("3");
        TestNode v8 = new TestNode("8");
        TestNode v9 = new TestNode("9");
        TestNode v4 = new TestNode("4");
        TestNode v5 = new TestNode("5");

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
        graph.addEdge(v1, v2);
        graph.addEdge(v2, v3);
        graph.addEdge(v3, v4);
        graph.addEdge(v4, v5);
        graph.addEdge(v5, v6);
        graph.addEdge(v5, v7);
        graph.addEdge(v7, v8);
        graph.addEdge(v8, v9);
        graph.addEdge(v0, v6);
        graph.addEdge(v1, v7);
        graph.addEdge(v1, v3);
        graph.addEdge(v1, v9);
        graph.addEdge(v1, v5);
        graph.addEdge(v7, v3);
        graph.addEdge(v3, v9);
        graph.addEdge(v9, v5);
        graph.addEdge(v4, v3);
        graph.addEdge(v5, v1);

        DepthFirstSearchOrderingTask<TestNode, DefaultEdge> task = new DepthFirstSearchOrderingTask<>(v0, graph, 1, DefaultEdge.class);
        DepthFirstSpanningTree<TestNode, DefaultEdge> spanningTree = task.run();
        String draw = new MermaidGraph<TestNode, DefaultEdge>().draw(spanningTree.sourceGraph());
        ClassifiedEdges<DefaultEdge> classifiedEdges = spanningTree.classifiedEdges();
        assertEquals(4, classifiedEdges.backEdges().size());
        assertTrue(classifiedEdges.backEdges().contains(graph.getEdge(v9, v5)));
    }
}
