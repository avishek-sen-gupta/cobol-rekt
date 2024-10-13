package org.smojol.toolkit.analysis.task;

import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.graph.DepthFirstSpanningTree;
import org.smojol.common.graph.DepthFirstSearchOrderingTask;
import org.smojol.common.graph.DominatorTree;
import org.smojol.common.graph.TestNode;
import org.smojol.toolkit.analysis.task.transpiler.BuildDominatorTreeTask;
import org.smojol.toolkit.analysis.task.transpiler.BuildDominatorsTask;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class BuildDominatorTreeTaskTest {
    /**
     *  @see <a href="documentation/dj-tree-unit-test-graph-1.png">Flowgraph for this test case</a>
     */
    @Test
    public void canBuildDominatorTree1() {
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

        DepthFirstSearchOrderingTask<TestNode, DefaultEdge> dfsTask = new DepthFirstSearchOrderingTask<>(vSTART, graph, DefaultEdge.class);
        DepthFirstSpanningTree<TestNode, DefaultEdge> spanningTree = dfsTask.run();

        List<Pair<TestNode, TestNode>> immediateDominators = new BuildDominatorsTask<TestNode, DefaultEdge>().immediateDominators(spanningTree);
        DominatorTree<TestNode, DefaultEdge> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, spanningTree.sourceGraphRoot(), DefaultEdge.class).run();
        assertEquals(9, dominatorTree.graph().edgeSet().size());
        assertEdgeDoesNotExistOfType(vSTART, vSTART, dominatorTree);
        assertEdgeExistsOfType(vSTART, vEND, dominatorTree);
        assertEdgeExistsOfType(vSTART, vA, dominatorTree);
        assertEdgeExistsOfType(vA, vB, dominatorTree);
        assertEdgeExistsOfType(vA, vC, dominatorTree);
        assertEdgeExistsOfType(vA, vD, dominatorTree);
        assertEdgeExistsOfType(vA, vF, dominatorTree);
        assertEdgeExistsOfType(vA, vH, dominatorTree);
        assertEdgeExistsOfType(vC, vE, dominatorTree);
        assertEdgeExistsOfType(vD, vG, dominatorTree);
    }

    /**
     *  @see <a href="documentation/dj-tree-unit-test-graph-2.png">Flowgraph for this test case</a>
     */
    @Test
    public void canBuildDominatorTree2() {
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

        DepthFirstSearchOrderingTask<TestNode, DefaultEdge> dfsTask = new DepthFirstSearchOrderingTask<>(vA, graph, DefaultEdge.class);
        DepthFirstSpanningTree<TestNode, DefaultEdge> spanningTree = dfsTask.run();

        List<Pair<TestNode, TestNode>> immediateDominators = new BuildDominatorsTask<TestNode, DefaultEdge>().immediateDominators(spanningTree);
        DominatorTree<TestNode, DefaultEdge> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, spanningTree.sourceGraphRoot(), DefaultEdge.class).run();
        assertEquals(4, dominatorTree.graph().edgeSet().size());
        assertEdgeDoesNotExistOfType(vA, vA, dominatorTree);
        assertEdgeExistsOfType(vA, vB, dominatorTree);
        assertEdgeExistsOfType(vA, vC, dominatorTree);
        assertEdgeExistsOfType(vB, vD, dominatorTree);
        assertEdgeExistsOfType(vC, vE, dominatorTree);
    }

    private void assertEdgeExistsOfType(TestNode from, TestNode to, DominatorTree<TestNode, DefaultEdge> dominatorTree) {
        assertEdgeOfTypeExistsOrNot(from, to, dominatorTree, true);
    }

    private void assertEdgeDoesNotExistOfType(TestNode from, TestNode to, DominatorTree<TestNode, DefaultEdge> dominatorTree) {
        assertEdgeOfTypeExistsOrNot(from, to, dominatorTree, false);
    }

    private void assertEdgeOfTypeExistsOrNot(TestNode from, TestNode to, DominatorTree<TestNode, DefaultEdge> dominatorTree, boolean exists) {
        Graph<TestNode, DefaultEdge> dominatorGraph = dominatorTree.graph();
        List<DefaultEdge> matchingEdges = dominatorGraph.edgeSet().stream().filter(e -> e.getClass() == DefaultEdge.class && dominatorGraph.getEdgeSource(e) == from && dominatorGraph.getEdgeTarget(e) == to).toList();
        assertEquals(exists ? 1 : 0, matchingEdges.size());
    }

    private static TestNode node(String id) {
        return new TestNode(id);
    }
}
