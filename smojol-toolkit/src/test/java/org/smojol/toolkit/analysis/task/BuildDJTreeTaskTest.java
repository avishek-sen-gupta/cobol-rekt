package org.smojol.toolkit.analysis.task;

import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.graph.DJTree;
import org.smojol.common.graph.DepthFirstSpanningTree;
import org.smojol.common.graph.DepthFirstTraversalLabelTask;
import org.smojol.common.graph.DominatorTree;
import org.smojol.toolkit.analysis.task.transpiler.*;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class BuildDJTreeTaskTest {
    /**
     *  @see <a href="documentation/dj-tree-unit-test-graph.png">Flowgraph for this test case</a>
     */
    @Test
    public void canBuildDJTree() {
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

        DepthFirstTraversalLabelTask<DominatorTreeTestNode, DefaultEdge> dfsTask = new DepthFirstTraversalLabelTask<>(vSTART, graph);
        DepthFirstSpanningTree<DominatorTreeTestNode, DefaultEdge> spanningTree = dfsTask.run();

        List<Pair<DominatorTreeTestNode, DominatorTreeTestNode>> immediateDominators = new BuildDominatorsTask<DominatorTreeTestNode, DefaultEdge>().immediateDominators(spanningTree);
        DominatorTree<DominatorTreeTestNode, DefaultEdge> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, spanningTree.sourceGraphRoot(), DefaultEdge.class).run();
        DJTree<DominatorTreeTestNode> djTree = new BuildDJTreeTask<>(dominatorTree, spanningTree).run();

        assertEdgeExistsOfType(vSTART, vEND, djTree, DominatorEdge.class);
        assertEdgeExistsOfType(vSTART, vA, djTree, DominatorEdge.class);
        assertEdgeExistsOfType(vA, vB, djTree, DominatorEdge.class);
        assertEdgeExistsOfType(vA, vC, djTree, DominatorEdge.class);
        assertEdgeExistsOfType(vA, vD, djTree, DominatorEdge.class);
        assertEdgeExistsOfType(vA, vF, djTree, DominatorEdge.class);
        assertEdgeExistsOfType(vA, vH, djTree, DominatorEdge.class);
        assertEdgeExistsOfType(vC, vE, djTree, DominatorEdge.class);
        assertEdgeExistsOfType(vD, vG, djTree, DominatorEdge.class);

        assertEdgeExistsOfType(vH, vEND, djTree, JoinEdge.class);
        assertEdgeExistsOfType(vH, vA, djTree, JoinEdge.class);
        assertEdgeExistsOfType(vH, vC, djTree, JoinEdge.class);
        assertEdgeExistsOfType(vC, vD, djTree, JoinEdge.class);
        assertEdgeExistsOfType(vD, vF, djTree, JoinEdge.class);
        assertEdgeExistsOfType(vF, vH, djTree, JoinEdge.class);
        assertEdgeExistsOfType(vB, vD, djTree, JoinEdge.class);
        assertEdgeExistsOfType(vE, vF, djTree, JoinEdge.class);
        assertEdgeExistsOfType(vG, vD, djTree, JoinEdge.class);
        assertEdgeExistsOfType(vG, vH, djTree, JoinEdge.class);
    }

    private void assertEdgeExistsOfType(DominatorTreeTestNode from, DominatorTreeTestNode to, DJTree<DominatorTreeTestNode> dominatorTree, Class<? extends DefaultEdge> edgeClass) {
        Graph<DominatorTreeTestNode, DefaultEdge> dominatorGraph = dominatorTree.graph();
        List<DefaultEdge> matchingEdges = dominatorGraph.edgeSet().stream().filter(e -> e.getClass() == edgeClass && dominatorGraph.getEdgeSource(e) == from && dominatorGraph.getEdgeTarget(e) == to).toList();
        assertEquals(1, matchingEdges.size());
    }

    private static DominatorTreeTestNode node(String id) {
        return new DominatorTreeTestNode(id);
    }
}

