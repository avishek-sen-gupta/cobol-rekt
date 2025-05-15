package org.smojol.toolkit.analysis.task;

import com.mojo.algorithms.graph.*;
import com.mojo.algorithms.transpiler.*;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

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
        Map<TestNode, Set<TestNode>> allDominators = new BuildDominatorsTask<TestNode, DefaultEdge>().allDominators(spanningTree.preOrdered(), graph);
        DominatorTree<TestNode, DefaultEdge> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, spanningTree.sourceGraphRoot(), DefaultEdge.class).run();
        DJTree<TestNode, DefaultEdge> djTree = new BuildDJTreeTask<>(dominatorTree, spanningTree, allDominators, DefaultEdge.class).run();

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

        DepthFirstSearchOrderingTask<TestNode, DefaultEdge> dfsTaskOnDJTree = new DepthFirstSearchOrderingTask<>(djTree.root(), djTree.graph(), DefaultEdge.class);
        DepthFirstSpanningTree<TestNode, DefaultEdge> djSpanningTree = dfsTaskOnDJTree.run();
        ClassifiedEdges<DefaultEdge> classifiedEdges = djSpanningTree.classifiedEdges();
        Set<DefaultEdge> backEdges = classifiedEdges.backEdges();
        assertTrue(backEdges.stream().anyMatch(be -> be.getClass() == BackJoinEdge.class));
    }

    /**
     * @see <a href="documentation/dj-tree-unit-test-graph-2.png">Flowgraph for this test case</a>
     */
    @Test
    public void canBuildDJTree2() {
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
        Map<TestNode, Set<TestNode>> allDominators = new BuildDominatorsTask<TestNode, DefaultEdge>().allDominators(spanningTree.preOrdered(), graph);
        DominatorTree<TestNode, DefaultEdge> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, spanningTree.sourceGraphRoot(), DefaultEdge.class).run();
        DJTree<TestNode, DefaultEdge> djTree = new BuildDJTreeTask<>(dominatorTree, spanningTree, allDominators, DefaultEdge.class).run();
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

    @Test
    public void canDetectBackEdges() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode v0 = node("0");
        TestNode v6 = node("6");
        TestNode v1 = node("1");
        TestNode v2 = node("2");
        TestNode v7 = node("7");
        TestNode v3 = node("3");
        TestNode v8 = node("8");
        TestNode v9 = node("9");
        TestNode v4 = node("4");
        TestNode v5 = node("5");

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
        graph.addEdge(v3, v9);
        graph.addEdge(v9, v5);
        graph.addEdge(v0, v6);
        graph.addEdge(v1, v7);
        graph.addEdge(v7, v8);
        graph.addEdge(v8, v9);
        graph.addEdge(v7, v3);
        graph.addEdge(v3, v4);
        graph.addEdge(v4, v3);
        graph.addEdge(v4, v5);
        graph.addEdge(v5, v1);
        graph.addEdge(v5, v6);
        graph.addEdge(v5, v7);

        DepthFirstSearchOrderingTask<TestNode, DefaultEdge> dfsTask = new DepthFirstSearchOrderingTask<>(v0, graph, DefaultEdge.class);
        DepthFirstSpanningTree<TestNode, DefaultEdge> spanningTree = dfsTask.run();

        List<Pair<TestNode, TestNode>> immediateDominators = new BuildDominatorsTask<TestNode, DefaultEdge>().immediateDominators(spanningTree);
        Map<TestNode, Set<TestNode>> allDominators = new BuildDominatorsTask<TestNode, DefaultEdge>().allDominators(spanningTree.preOrdered(), graph);
        DominatorTree<TestNode, DefaultEdge> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, spanningTree.sourceGraphRoot(), DefaultEdge.class).run();
        DJTree<TestNode, DefaultEdge> djTree = new BuildDJTreeTask<>(dominatorTree, spanningTree, allDominators, DefaultEdge.class).run();
        String draw = new MermaidGraph<TestNode, DefaultEdge>().draw(djTree.graph());
        DepthFirstSearchOrderingTask<TestNode, DefaultEdge> dfsTaskOnDJTree = new DepthFirstSearchOrderingTask<>(djTree.root(), djTree.graph(), DefaultEdge.class);
        DepthFirstSpanningTree<TestNode, DefaultEdge> djSpanningTree = dfsTaskOnDJTree.run();
        List<TestNode> ordering = djSpanningTree.preOrdered();
        ClassifiedEdges<DefaultEdge> classifiedEdges = djSpanningTree.classifiedEdges();
        Graph<TestNode, DefaultEdge> djGraph = djTree.graph();
        String draw1 = new MermaidGraph<TestNode, DefaultEdge>().draw(djGraph);
        assertEdgeExistsOfType(v5, v1, djTree, IS_JOIN_EDGE);
        Set<DefaultEdge> backJoinEdges = djTree.graph().edgeSet().stream().filter(IS_BACK_JOIN_EDGE::apply).collect(Collectors.toUnmodifiableSet());
        Set<DefaultEdge> crossJoinEdges = djTree.graph().edgeSet().stream().filter(IS_CROSS_JOIN_EDGE::apply).collect(Collectors.toUnmodifiableSet());
        assertFalse(backJoinEdges.contains(djGraph.getEdge(v9, v5)));
        assertTrue(classifiedEdges.backEdges().contains(djGraph.getEdge(v9, v5)));
    }

    private void assertEdgeExistsOfType(TestNode from, TestNode to, DJTree<TestNode, DefaultEdge> dominatorTree, Function<DefaultEdge, Boolean> condition) {
        Graph<TestNode, DefaultEdge> dominatorGraph = dominatorTree.graph();
        List<DefaultEdge> matchingEdges = dominatorGraph.edgeSet().stream().filter(e -> condition.apply(e) && dominatorGraph.getEdgeSource(e) == from && dominatorGraph.getEdgeTarget(e) == to).toList();
        assertEquals(1, matchingEdges.size());
    }

    private static TestNode node(String id) {
        return new TestNode(id);
    }
}

