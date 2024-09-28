package org.smojol.toolkit.analysis;

import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import org.smojol.common.id.Identifiable;
import org.smojol.common.transpiler.FlowgraphReductionResult;
import org.smojol.common.transpiler.FlowgraphTransformer;
import org.smojol.toolkit.analysis.defined.IrreducibleRegionsTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;

import java.util.List;
import java.util.Set;
import java.util.function.BiFunction;

import static org.junit.jupiter.api.Assertions.assertEquals;

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
}
