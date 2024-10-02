package org.smojol.toolkit.analysis;

import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.id.Identifiable;
import org.smojol.common.transpiler.FlowgraphReductionResult;
import org.smojol.common.transpiler.FlowgraphTransformer;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.toolkit.analysis.task.transpiler.IntervalAnalysisTask;
import org.smojol.toolkit.analysis.task.transpiler.IrreducibleStronglyConnectedComponentsTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;

import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.BiFunction;

import static org.junit.jupiter.api.Assertions.*;

public class ReducibleFlowgraphTest {
    @Test
    public void canMergeNodes() {
        Graph<TestNode, String> graph = new DefaultDirectedGraph<>(String.class);
        BiFunction<TestNode, TestNode, String> buildEdge = (v1, v2) -> String.format("(%s, %s)", v1, v2);
        FlowgraphTransformer<TestNode, String> transformer = new FlowgraphTransformer<>(graph, buildEdge, v -> v.label().equals("1"));
        transformer.addVertex(node("1"));
        transformer.addVertex(node("2"));
        transformer.addVertex(node("3"));
        transformer.addVertex(node("4"));
        transformer.addVertex(node("5"));
        transformer.addEdge(node("1"), node("1"));
        transformer.addEdge(node("1"), node("2"));
        transformer.addEdge(node("1"), node("3"));
        transformer.addEdge(node("2"), node("3"));
        transformer.addEdge(node("3"), node("2"));
        transformer.addEdge(node("2"), node("4"));
        transformer.addEdge(node("3"), node("5"));
        FlowgraphReductionResult<TestNode, String> evolutions = transformer.reduce();
    }

    @Test
    public void testReducibilityOfFlowgraphUsingIntervalAnalysisAndStronglyConnectedComponents() {
        testReducibilityUsingBothMethods(nonReducibleGraph1(), false);
        testReducibilityUsingBothMethods(reducibleGraph(), true);
        testReducibilityUsingBothMethods(multipleEntryMultiplePredecessorSCC(), false);
        testReducibilityUsingBothMethods(gotoIntoLoop(), false);
        testReducibilityUsingBothMethods(simpleNonReducibleGraph(), false);
        testReducibilityUsingBothMethods(counterExample(), true, false);
    }

    /*
         ┌-----------┐
         ▼           ▼
    1 -> 2 <-> 3 <-> 4

     */
    private Graph<TestNode, DefaultEdge> counterExample() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        graph.addVertex(node("1"));
        graph.addVertex(node("2"));
        graph.addVertex(node("3"));
        graph.addVertex(node("4"));
        graph.addEdge(node("1"), node("2"));
        graph.addEdge(node("2"), node("3"));
        graph.addEdge(node("3"), node("2"));
        graph.addEdge(node("2"), node("4"));
        graph.addEdge(node("4"), node("2"));
        graph.addEdge(node("3"), node("4"));
        graph.addEdge(node("4"), node("3"));
        return graph;
    }

    private Graph<TestNode, DefaultEdge> simpleNonReducibleGraph() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        graph.addVertex(node("1"));
        graph.addVertex(node("2"));
        graph.addVertex(node("3"));
        graph.addVertex(node("4"));
        graph.addVertex(node("5"));
        graph.addEdge(node("1"), node("2"));
        graph.addEdge(node("2"), node("3"));
        graph.addEdge(node("3"), node("4"));
        graph.addEdge(node("4"), node("5"));
        graph.addEdge(node("1"), node("3"));
        graph.addEdge(node("4"), node("2"));
        return graph;
    }

    private static void testReducibilityUsingBothMethods(Graph<TestNode, DefaultEdge> graph, boolean shouldBeReducible) {
        testReducibilityUsingBothMethods(graph, shouldBeReducible, shouldBeReducible);
    }
    private static void testReducibilityUsingBothMethods(Graph<TestNode, DefaultEdge> graph, boolean noImproperSCCs, boolean shouldBeReducible) {
        AnalysisTaskResult result = new IrreducibleStronglyConnectedComponentsTask<>(graph).run();
        List<Pair<Graph<TestNode, DefaultEdge>, Set<DefaultEdge>>> badSCCs = ((AnalysisTaskResultOK) result).getDetail();
        assertEquals(noImproperSCCs, badSCCs.isEmpty());
        AnalysisTaskResult secondReducibleTest = new IntervalAnalysisTask<>(graph, n -> n.equals(node("1")), (a, b) -> new DefaultEdge()).run();
        FlowgraphReductionResult<TranspilerInstruction, DefaultEdge> reductions = ((AnalysisTaskResultOK) secondReducibleTest).getDetail();
        assertEquals(shouldBeReducible, reductions.isReducible());
    }

    private static Graph<TestNode, DefaultEdge> gotoIntoLoop() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        graph.addVertex(node("1"));
        graph.addVertex(node("2"));
        graph.addVertex(node("3"));
        graph.addVertex(node("4"));
        graph.addEdge(node("1"), node("2"));
        graph.addEdge(node("2"), node("3"));
        graph.addEdge(node("2"), node("4"));
        graph.addEdge(node("3"), node("2"));
        graph.addEdge(node("1"), node("3"));
        return graph;
    }

    private static Graph<TestNode, DefaultEdge> reducibleGraph() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode node1 = node("1");
        TestNode node2 = node("2");
        TestNode node3 = node("3");
        TestNode node4 = node("4");
        TestNode node5 = node("5");

        graph.addVertex(node1);
        graph.addVertex(node2);
        graph.addVertex(node3);
        graph.addVertex(node4);
        graph.addVertex(node5);
        graph.addEdge(node1, node2);
        graph.addEdge(node2, node3);
        graph.addEdge(node3, node4);
        graph.addEdge(node4, node1);
        graph.addEdge(node1, node5);
        graph.addEdge(node5, node2);
        return graph;
    }

    private static Graph<TestNode, DefaultEdge> multipleEntryMultiplePredecessorSCC() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode node1 = node("1");
        TestNode node2 = node("2");
        TestNode node3 = node("3");
        TestNode node4 = node("4");
        TestNode node5 = node("5");
        TestNode node6 = node("6");

        graph.addVertex(node1);
        graph.addVertex(node2);
        graph.addVertex(node3);
        graph.addVertex(node4);
        graph.addVertex(node5);
        graph.addVertex(node6);

        graph.addEdge(node1, node2);
        graph.addEdge(node1, node3);
        graph.addEdge(node2, node4);
        graph.addEdge(node3, node5);
        graph.addEdge(node4, node5);
        graph.addEdge(node5, node6);
        graph.addEdge(node6, node4);
        return graph;
    }

    private static Graph<TestNode, DefaultEdge> nonReducibleGraph1() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        graph.addVertex(node("1"));
        graph.addVertex(node("2"));
        graph.addVertex(node("3"));
        graph.addVertex(node("4"));
        graph.addVertex(node("5"));
        graph.addEdge(node("1"), node("2"));
        graph.addEdge(node("1"), node("3"));
        graph.addEdge(node("2"), node("3"));
        graph.addEdge(node("3"), node("2"));
        graph.addEdge(node("2"), node("4"));
        graph.addEdge(node("3"), node("5"));
        return graph;
    }

    private static TestNode node(String id) {
        return new TestNode(id);
    }
}

record TestNode(String id) implements Identifiable {
    @Override
    public String label() {
        return id;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof TestNode testNode)) return false;
        return Objects.equals(id, testNode.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
