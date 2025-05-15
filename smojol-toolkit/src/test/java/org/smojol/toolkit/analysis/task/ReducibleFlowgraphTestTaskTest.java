package org.smojol.toolkit.analysis.task;

import com.mojo.algorithms.graph.TestNode;
import com.mojo.algorithms.transpiler.ReducibleFlowgraphTestTask;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ReducibleFlowgraphTestTaskTest {
    @Test
    public void canDetectReducibleGraphWithoutLoops() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode vA = node("A");
        TestNode vB = node("B");
        TestNode vC = node("C");

        graph.addVertex(vA);
        graph.addVertex(vB);
        graph.addVertex(vC);

        graph.addEdge(vA, vB);
        graph.addEdge(vA, vC);

        assertTrue(new ReducibleFlowgraphTestTask<>(vA, graph, DefaultEdge.class).run());
    }
    @Test

    public void canDetectReducibleGraphWithLoops() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode vA = node("A");
        TestNode vB = node("B");
        TestNode vC = node("C");

        graph.addVertex(vA);
        graph.addVertex(vB);
        graph.addVertex(vC);

        graph.addEdge(vA, vB);
        graph.addEdge(vB, vC);
        graph.addEdge(vC, vA);

        assertTrue(new ReducibleFlowgraphTestTask<>(vA, graph, DefaultEdge.class).run());
    }

    /**
     * @see <a href="documentation/dj-tree-unit-test-graph-1.png">Flowgraph for this test case</a>
     */
    @Test
    public void canDetectIrreducibleGraph1() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode vSTART = node("START");
        TestNode vEND = node("END");
        TestNode vA = node("A");
        TestNode vB = node("B");
        TestNode vC = node("C");
        TestNode vD = node("D");
        TestNode vE = node("E");
        TestNode vF = node("F");
        TestNode vG = node("G");
        TestNode vH = node("H");

        graph.addVertex(vSTART);
        graph.addVertex(vEND);
        graph.addVertex(vA);
        graph.addVertex(vB);
        graph.addVertex(vC);
        graph.addVertex(vD);
        graph.addVertex(vE);
        graph.addVertex(vF);
        graph.addVertex(vG);
        graph.addVertex(vH);

        graph.addEdge(vSTART, vA);
        graph.addEdge(vSTART, vEND);
        graph.addEdge(vA, vB);
        graph.addEdge(vA, vC);
        graph.addEdge(vB, vD);
        graph.addEdge(vC, vD);
        graph.addEdge(vC, vE);
        graph.addEdge(vE, vF);
        graph.addEdge(vD, vG);
        graph.addEdge(vD, vF);
        graph.addEdge(vG, vD);
        graph.addEdge(vG, vH);
        graph.addEdge(vF, vH);
        graph.addEdge(vH, vA);
        graph.addEdge(vH, vC);
        graph.addEdge(vH, vEND);

        assertFalse(new ReducibleFlowgraphTestTask<>(vSTART, graph, DefaultEdge.class).run());
    }

    /**
     * @see <a href="documentation/dj-tree-unit-test-graph-2.png">Flowgraph for this test case</a>
     */
    @Test
    public void canDetectIrreducibleGraph2() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode vA = node("A");
        TestNode vB = node("B");
        TestNode vC = node("C");
        TestNode vD = node("D");
        TestNode vE = node("E");

        graph.addVertex(vA);
        graph.addVertex(vB);
        graph.addVertex(vC);
        graph.addVertex(vD);
        graph.addVertex(vE);

        graph.addEdge(vA, vB);
        graph.addEdge(vA, vC);
        graph.addEdge(vB, vC);
        graph.addEdge(vC, vB);
        graph.addEdge(vB, vD);
        graph.addEdge(vD, vB);
        graph.addEdge(vC, vE);
        graph.addEdge(vE, vC);

        assertFalse(new ReducibleFlowgraphTestTask<>(vA, graph, DefaultEdge.class).run());
    }

    /*
         ┌-----------┐
         ▼           ▼
    1 -> 2 <-> 3 <-> 4

    */

    @Test
    public void doesNotGiveFalsePositives() {
        Pair<TestNode, Graph<TestNode, DefaultEdge>> example = falsePositiveExample();
        assertFalse(new ReducibleFlowgraphTestTask<>(example.getLeft(), example.getRight(), DefaultEdge.class).run());
    }

    private Pair<TestNode, Graph<TestNode, DefaultEdge>> falsePositiveExample() {
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
        graph.addEdge(v2, v3);
        graph.addEdge(v3, v2);
        graph.addEdge(v2, v4);
        graph.addEdge(v4, v2);
        graph.addEdge(v3, v4);
        graph.addEdge(v4, v3);
        return ImmutablePair.of(v1, graph);
    }

    private static TestNode node(String id) {
        return new TestNode(id);
    }
}
