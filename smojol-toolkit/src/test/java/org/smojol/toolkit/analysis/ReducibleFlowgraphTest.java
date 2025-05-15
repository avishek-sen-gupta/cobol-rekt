package org.smojol.toolkit.analysis;

import com.mojo.algorithms.graph.TestNode;
import com.mojo.algorithms.transpiler.FlowgraphReductionResult;
import com.mojo.algorithms.transpiler.FlowgraphTransformer;
import com.mojo.algorithms.transpiler.IrreducibleStronglyConnectedComponentsTask;
import com.mojo.algorithms.transpiler.TranspilerInstruction;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.AnalysisTaskResultOK;
import org.smojol.toolkit.task.T1_T2_IntervalAnalysisTask;

import java.util.List;
import java.util.Set;
import java.util.function.BiFunction;

import static org.junit.jupiter.api.Assertions.*;

public class ReducibleFlowgraphTest {
    @Test
    public void canMergeNodes() {
        Graph<TestNode, String> graph = new DefaultDirectedGraph<>(String.class);
        BiFunction<TestNode, TestNode, String> buildEdge = (v1, v2) -> String.format("(%s, %s)", v1, v2);
        FlowgraphTransformer<TestNode, String> transformer = new FlowgraphTransformer<>(graph, buildEdge, v -> v.label().equals("1"));
        TestNode v1 = node("1");
        TestNode v2 = node("2");
        TestNode v3 = node("3");
        TestNode v4 = node("4");
        TestNode v5 = node("5");

        transformer.addVertex(v1);
        transformer.addVertex(v2);
        transformer.addVertex(v3);
        transformer.addVertex(v4);
        transformer.addVertex(v5);
        transformer.addEdge(v1, v1);
        transformer.addEdge(v1, v2);
        transformer.addEdge(v1, v3);
        transformer.addEdge(v2, v3);
        transformer.addEdge(v3, v2);
        transformer.addEdge(v2, v4);
        transformer.addEdge(v3, v5);
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
        return graph;
    }

    private Graph<TestNode, DefaultEdge> simpleNonReducibleGraph() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode v1 = node("1");
        TestNode v2 = node("2");
        TestNode v3 = node("3");
        TestNode v4 = node("4");
        TestNode v5 = node("5");
        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addVertex(v3);
        graph.addVertex(v4);
        graph.addVertex(v5);
        graph.addEdge(v1, v2);
        graph.addEdge(v2, v3);
        graph.addEdge(v3, v4);
        graph.addEdge(v4, v5);
        graph.addEdge(v1, v3);
        graph.addEdge(v4, v2);
        return graph;
    }

    private static void testReducibilityUsingBothMethods(Graph<TestNode, DefaultEdge> graph, boolean shouldBeReducible) {
        testReducibilityUsingBothMethods(graph, shouldBeReducible, shouldBeReducible);
    }
    private static void testReducibilityUsingBothMethods(Graph<TestNode, DefaultEdge> graph, boolean noImproperSCCs, boolean shouldBeReducible) {
        AnalysisTaskResult result = new IrreducibleStronglyConnectedComponentsTask<>(graph).run();
        List<Pair<Graph<TestNode, DefaultEdge>, Set<DefaultEdge>>> badSCCs = ((AnalysisTaskResultOK) result).getDetail();
        assertEquals(noImproperSCCs, badSCCs.isEmpty());
        AnalysisTaskResult secondReducibleTest = new T1_T2_IntervalAnalysisTask<>(graph, n -> n.equals(node("1")), (a, b) -> new DefaultEdge()).run();
        FlowgraphReductionResult<TranspilerInstruction, DefaultEdge> reductions = ((AnalysisTaskResultOK) secondReducibleTest).getDetail();
        assertEquals(shouldBeReducible, reductions.isReducible());
    }

    private static Graph<TestNode, DefaultEdge> gotoIntoLoop() {
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
        graph.addEdge(v2, v4);
        graph.addEdge(v3, v2);
        graph.addEdge(v1, v3);
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
        TestNode v1 = node("1");
        TestNode v2 = node("2");
        TestNode v3 = node("3");
        TestNode v4 = node("4");
        TestNode v5 = node("5");
        TestNode v6 = node("6");

        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addVertex(v3);
        graph.addVertex(v4);
        graph.addVertex(v5);
        graph.addVertex(v6);

        graph.addEdge(v1, v2);
        graph.addEdge(v1, v3);
        graph.addEdge(v2, v4);
        graph.addEdge(v3, v5);
        graph.addEdge(v4, v5);
        graph.addEdge(v5, v6);
        graph.addEdge(v6, v4);
        return graph;
    }

    private static Graph<TestNode, DefaultEdge> nonReducibleGraph1() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        TestNode v1 = node("1");
        TestNode v2 = node("2");
        TestNode v3 = node("3");
        TestNode v4 = node("4");
        TestNode v5 = node("5");
        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addVertex(v3);
        graph.addVertex(v4);
        graph.addVertex(v5);
        graph.addEdge(v1, v2);
        graph.addEdge(v1, v3);
        graph.addEdge(v2, v3);
        graph.addEdge(v3, v2);
        graph.addEdge(v2, v4);
        graph.addEdge(v3, v5);
        return graph;
    }

    private static TestNode node(String id) {
        return new TestNode(id);
    }
}
