package org.smojol.toolkit.analysis.task;

import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.graph.DepthFirstSpanningTree;
import org.smojol.common.graph.DepthFirstTraversalLabelTask;
import org.smojol.common.graph.DominatorTree;
import org.smojol.common.id.Identifiable;
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

        DepthFirstTraversalLabelTask<DominatorTreeTestNode, DefaultEdge> dfsTask = new DepthFirstTraversalLabelTask<>(vSTART, graph, DefaultEdge.class);
        DepthFirstSpanningTree<DominatorTreeTestNode, DefaultEdge> spanningTree = dfsTask.run();

        List<Pair<DominatorTreeTestNode, DominatorTreeTestNode>> immediateDominators = new BuildDominatorsTask<DominatorTreeTestNode, DefaultEdge>().immediateDominators(spanningTree);
        DominatorTree<DominatorTreeTestNode, DefaultEdge> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, spanningTree.sourceGraphRoot(), DefaultEdge.class).run();
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

        DepthFirstTraversalLabelTask<DominatorTreeTestNode, DefaultEdge> dfsTask = new DepthFirstTraversalLabelTask<>(vA, graph, DefaultEdge.class);
        DepthFirstSpanningTree<DominatorTreeTestNode, DefaultEdge> spanningTree = dfsTask.run();

        List<Pair<DominatorTreeTestNode, DominatorTreeTestNode>> immediateDominators = new BuildDominatorsTask<DominatorTreeTestNode, DefaultEdge>().immediateDominators(spanningTree);
        DominatorTree<DominatorTreeTestNode, DefaultEdge> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, spanningTree.sourceGraphRoot(), DefaultEdge.class).run();
        assertEquals(4, dominatorTree.graph().edgeSet().size());
        assertEdgeDoesNotExistOfType(vA, vA, dominatorTree);
        assertEdgeExistsOfType(vA, vB, dominatorTree);
        assertEdgeExistsOfType(vA, vC, dominatorTree);
        assertEdgeExistsOfType(vB, vD, dominatorTree);
        assertEdgeExistsOfType(vC, vE, dominatorTree);
    }

    private void assertEdgeExistsOfType(DominatorTreeTestNode from, DominatorTreeTestNode to, DominatorTree<DominatorTreeTestNode, DefaultEdge> dominatorTree) {
        assertEdgeOfTypeExistsOrNot(from, to, dominatorTree, true);
    }

    private void assertEdgeDoesNotExistOfType(DominatorTreeTestNode from, DominatorTreeTestNode to, DominatorTree<DominatorTreeTestNode, DefaultEdge> dominatorTree) {
        assertEdgeOfTypeExistsOrNot(from, to, dominatorTree, false);
    }

    private void assertEdgeOfTypeExistsOrNot(DominatorTreeTestNode from, DominatorTreeTestNode to, DominatorTree<DominatorTreeTestNode, DefaultEdge> dominatorTree, boolean exists) {
        Graph<DominatorTreeTestNode, DefaultEdge> dominatorGraph = dominatorTree.graph();
        List<DefaultEdge> matchingEdges = dominatorGraph.edgeSet().stream().filter(e -> e.getClass() == DefaultEdge.class && dominatorGraph.getEdgeSource(e) == from && dominatorGraph.getEdgeTarget(e) == to).toList();
        assertEquals(exists ? 1 : 0, matchingEdges.size());
    }

    private static DominatorTreeTestNode node(String id) {
        return new DominatorTreeTestNode(id);
    }
}

record DominatorTreeTestNode(String id) implements Identifiable {
    @Override
    public String label() {
        return id;
    }
}

