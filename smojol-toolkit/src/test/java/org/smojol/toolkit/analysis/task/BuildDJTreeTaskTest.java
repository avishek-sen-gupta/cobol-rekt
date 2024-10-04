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
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class BuildDJTreeTaskTest {
    private static final Function<DefaultEdge, Boolean> IS_DOMINATOR_EDGE = v -> v instanceof DominatorEdge;
    private static final Function<DefaultEdge, Boolean> IS_JOIN_EDGE = v -> v instanceof JoinEdge;
    private static final Function<DefaultEdge, Boolean> IS_BACK_JOIN_EDGE = v -> v instanceof BackJoinEdge;
    private static final Function<DefaultEdge, Boolean> IS_CROSS_JOIN_EDGE = v -> v instanceof CrossJoinEdge;

    /**
     * @see <a href="documentation/dj-tree-unit-test-graph-1.png">Flowgraph for this test case</a>
     */
    @Test
    public void canBuildDJTree1() {
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
        Map<DominatorTreeTestNode, Set<DominatorTreeTestNode>> allDominators = new BuildDominatorsTask<DominatorTreeTestNode, DefaultEdge>().allDominators(spanningTree.preOrder(), graph);
        DominatorTree<DominatorTreeTestNode, DefaultEdge> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, spanningTree.sourceGraphRoot(), DefaultEdge.class).run();
        DJTree<DominatorTreeTestNode> djTree = new BuildDJTreeTask<>(dominatorTree, spanningTree, immediateDominators, allDominators).run();

        assertEquals(9, djTree.graph().edgeSet().stream().filter(IS_DOMINATOR_EDGE::apply).count());
        assertEdgeExistsOfType(vSTART, vEND, djTree, IS_DOMINATOR_EDGE);
        assertEdgeExistsOfType(vSTART, vA, djTree, IS_DOMINATOR_EDGE);
        assertEdgeExistsOfType(vA, vB, djTree, IS_DOMINATOR_EDGE);
        assertEdgeExistsOfType(vA, vC, djTree, IS_DOMINATOR_EDGE);
        assertEdgeExistsOfType(vA, vD, djTree, IS_DOMINATOR_EDGE);
        assertEdgeExistsOfType(vA, vF, djTree, IS_DOMINATOR_EDGE);
        assertEdgeExistsOfType(vA, vH, djTree, IS_DOMINATOR_EDGE);
        assertEdgeExistsOfType(vC, vE, djTree, IS_DOMINATOR_EDGE);
        assertEdgeExistsOfType(vD, vG, djTree, IS_DOMINATOR_EDGE);

        assertEquals(10, djTree.graph().edgeSet().stream().filter(IS_JOIN_EDGE::apply).count());
        assertEdgeExistsOfType(vH, vEND, djTree, IS_JOIN_EDGE);
        assertEdgeExistsOfType(vH, vA, djTree, IS_JOIN_EDGE);
        assertEdgeExistsOfType(vH, vC, djTree, IS_JOIN_EDGE);
        assertEdgeExistsOfType(vC, vD, djTree, IS_JOIN_EDGE);
        assertEdgeExistsOfType(vD, vF, djTree, IS_JOIN_EDGE);
        assertEdgeExistsOfType(vF, vH, djTree, IS_JOIN_EDGE);
        assertEdgeExistsOfType(vB, vD, djTree, IS_JOIN_EDGE);
        assertEdgeExistsOfType(vE, vF, djTree, IS_JOIN_EDGE);
        assertEdgeExistsOfType(vG, vD, djTree, IS_JOIN_EDGE);
        assertEdgeExistsOfType(vG, vH, djTree, IS_JOIN_EDGE);

        Set<DefaultEdge> backJoinEdges = djTree.graph().edgeSet().stream().filter(IS_BACK_JOIN_EDGE::apply).collect(Collectors.toUnmodifiableSet());
        Set<DefaultEdge> crossJoinEdges = djTree.graph().edgeSet().stream().filter(IS_CROSS_JOIN_EDGE::apply).collect(Collectors.toUnmodifiableSet());
        assertEquals(2, backJoinEdges.size());

        assertTrue(backJoinEdges.contains(djTree.graph().getEdge(vH, vA)));
        assertTrue(backJoinEdges.contains(djTree.graph().getEdge(vG, vD)));

        assertEquals(8, crossJoinEdges.size());
        assertTrue(crossJoinEdges.contains(djTree.graph().getEdge(vH, vEND)));
        assertTrue(crossJoinEdges.contains(djTree.graph().getEdge(vH, vC)));
        assertTrue(crossJoinEdges.contains(djTree.graph().getEdge(vC, vD)));
        assertTrue(crossJoinEdges.contains(djTree.graph().getEdge(vD, vF)));
        assertTrue(crossJoinEdges.contains(djTree.graph().getEdge(vF, vH)));
        assertTrue(crossJoinEdges.contains(djTree.graph().getEdge(vB, vD)));
        assertTrue(crossJoinEdges.contains(djTree.graph().getEdge(vG, vH)));
        assertTrue(crossJoinEdges.contains(djTree.graph().getEdge(vE, vF)));
    }

    /**
     * @see <a href="documentation/dj-tree-unit-test-graph-2.png">Flowgraph for this test case</a>
     */
    @Test
    public void canBuildDJTree2() {
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
        Map<DominatorTreeTestNode, Set<DominatorTreeTestNode>> allDominators = new BuildDominatorsTask<DominatorTreeTestNode, DefaultEdge>().allDominators(spanningTree.preOrder(), graph);
        DominatorTree<DominatorTreeTestNode, DefaultEdge> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, spanningTree.sourceGraphRoot(), DefaultEdge.class).run();
        DJTree<DominatorTreeTestNode> djTree = new BuildDJTreeTask<>(dominatorTree, spanningTree, immediateDominators, allDominators).run();
        assertEquals(4, djTree.graph().edgeSet().stream().filter(IS_DOMINATOR_EDGE::apply).count());
        assertEdgeExistsOfType(vA, vB, djTree, IS_DOMINATOR_EDGE);
        assertEdgeExistsOfType(vA, vC, djTree, IS_DOMINATOR_EDGE);
        assertEdgeExistsOfType(vB, vD, djTree, IS_DOMINATOR_EDGE);
        assertEdgeExistsOfType(vC, vE, djTree, IS_DOMINATOR_EDGE);

        assertEquals(4, djTree.graph().edgeSet().stream().filter(IS_JOIN_EDGE::apply).count());
        assertEdgeExistsOfType(vB, vC, djTree, IS_JOIN_EDGE);
        assertEdgeExistsOfType(vC, vB, djTree, IS_JOIN_EDGE);
        assertEdgeExistsOfType(vD, vB, djTree, IS_JOIN_EDGE);
        assertEdgeExistsOfType(vE, vC, djTree, IS_JOIN_EDGE);

        Set<DefaultEdge> backJoinEdges = djTree.graph().edgeSet().stream().filter(IS_BACK_JOIN_EDGE::apply).collect(Collectors.toUnmodifiableSet());
        Set<DefaultEdge> crossJoinEdges = djTree.graph().edgeSet().stream().filter(IS_CROSS_JOIN_EDGE::apply).collect(Collectors.toUnmodifiableSet());
        assertEquals(2, backJoinEdges.size());
        assertTrue(backJoinEdges.contains(djTree.graph().getEdge(vD, vB)));
        assertTrue(backJoinEdges.contains(djTree.graph().getEdge(vE, vC)));


        assertEquals(2, crossJoinEdges.size());
        assertTrue(crossJoinEdges.contains(djTree.graph().getEdge(vB, vC)));
        assertTrue(crossJoinEdges.contains(djTree.graph().getEdge(vC, vB)));
    }

    private void assertEdgeExistsOfType(DominatorTreeTestNode from, DominatorTreeTestNode to, DJTree<DominatorTreeTestNode> dominatorTree, Function<DefaultEdge, Boolean> condition) {
        Graph<DominatorTreeTestNode, DefaultEdge> dominatorGraph = dominatorTree.graph();
        List<DefaultEdge> matchingEdges = dominatorGraph.edgeSet().stream().filter(e -> condition.apply(e) && dominatorGraph.getEdgeSource(e) == from && dominatorGraph.getEdgeTarget(e) == to).toList();
        assertEquals(1, matchingEdges.size());
    }

    private static DominatorTreeTestNode node(String id) {
        return new DominatorTreeTestNode(id);
    }
}

