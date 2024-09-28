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
import org.smojol.toolkit.analysis.defined.IntervalAnalysisTask;
import org.smojol.toolkit.analysis.defined.IrreducibleRegionsTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;

import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.BiFunction;

import static org.junit.jupiter.api.Assertions.*;

public class IntervalAnalysisTest {
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
    public void findsImproperRegionsUsingStronglyConnectedComponents() {
        testNonReducibleFlowgraph(nonReducibleGraph1());
        testNonReducibleFlowgraph(nonReducibleGraph2());

    }

    private static void testNonReducibleFlowgraph(Graph<TestNode, DefaultEdge> nonReducibleGraph) {
        AnalysisTaskResult result = new IrreducibleRegionsTask<TestNode, DefaultEdge>().run(nonReducibleGraph);
        List<Pair<Graph<TestNode, DefaultEdge>, Set<DefaultEdge>>> badSCCs = ((AnalysisTaskResultOK) result).getDetail();
        assertEquals(1, badSCCs.size());
    }

    @Test
    public void testNonReducibleFlowgraphWithUsingIntervalNalaysisAndStronglyConnectedComponents() {
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
        AnalysisTaskResult result = new IrreducibleRegionsTask<TestNode, DefaultEdge>().run(graph);
        List<Pair<Graph<TestNode, DefaultEdge>, Set<DefaultEdge>>> badSCCs = ((AnalysisTaskResultOK) result).getDetail();
        assertEquals(0, badSCCs.size());
        AnalysisTaskResult secondReducibleTest = new IntervalAnalysisTask<>(graph, n -> n.equals(node1), (a, b) -> new DefaultEdge()).run();
        FlowgraphReductionResult<TranspilerInstruction, DefaultEdge> reductions = ((AnalysisTaskResultOK) secondReducibleTest).getDetail();
        assertTrue(reductions.isReducible());
    }

    private static Graph<TestNode, DefaultEdge> nonReducibleGraph2() {
        Graph<TestNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        graph.addVertex(node("1"));
        graph.addVertex(node("2"));
        graph.addVertex(node("3"));
        graph.addVertex(node("4"));
        graph.addVertex(node("5"));
        graph.addEdge(node("1"), node("2"));
        graph.addEdge(node("2"), node("3"));
        graph.addEdge(node("3"), node("4"));
        graph.addEdge(node("4"), node("1"));
        graph.addEdge(node("2"), node("4"));
        graph.addEdge(node("5"), node("1"));
        graph.addEdge(node("5"), node("2"));
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
