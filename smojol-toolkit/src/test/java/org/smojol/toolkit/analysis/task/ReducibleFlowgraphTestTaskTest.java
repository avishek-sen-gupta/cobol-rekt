package org.smojol.toolkit.analysis.task;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.toolkit.analysis.task.transpiler.*;

import static org.junit.jupiter.api.Assertions.*;

public class ReducibleFlowgraphTestTaskTest {
    /**
     * @see <a href="documentation/dj-tree-unit-test-graph-1.png">Flowgraph for this test case</a>
     */
    @Test
    public void canDetectIrreducibleGraph1() {
        Graph<DominatorTreeTestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        DominatorTreeTestNode vSTART = node("START");
        DominatorTreeTestNode vEND = node("END");
        DominatorTreeTestNode vA = node("A");
        DominatorTreeTestNode vB = node("B");
        DominatorTreeTestNode vC = node("C");
        DominatorTreeTestNode vD = node("D");
        DominatorTreeTestNode vE = node("E");
        DominatorTreeTestNode vF = node("F");
        DominatorTreeTestNode vG = node("G");
        DominatorTreeTestNode vH = node("H");

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
        Graph<DominatorTreeTestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        DominatorTreeTestNode vA = node("A");
        DominatorTreeTestNode vB = node("B");
        DominatorTreeTestNode vC = node("C");
        DominatorTreeTestNode vD = node("D");
        DominatorTreeTestNode vE = node("E");

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
        Pair<DominatorTreeTestNode, Graph<DominatorTreeTestNode, DefaultEdge>> example = falsePositiveExample();
        assertFalse(new ReducibleFlowgraphTestTask<>(example.getLeft(), example.getRight(), DefaultEdge.class).run());
    }

    private Pair<DominatorTreeTestNode, Graph<DominatorTreeTestNode, DefaultEdge>> falsePositiveExample() {
        Graph<DominatorTreeTestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        DominatorTreeTestNode v1 = node("1");
        DominatorTreeTestNode v2 = node("2");
        DominatorTreeTestNode v3 = node("3");
        DominatorTreeTestNode v4 = node("4");

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

    private static DominatorTreeTestNode node(String id) {
        return new DominatorTreeTestNode(id);
    }
}
