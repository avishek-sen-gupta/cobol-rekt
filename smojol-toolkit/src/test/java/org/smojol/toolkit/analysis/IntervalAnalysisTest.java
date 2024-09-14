package org.smojol.toolkit.analysis;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.junit.jupiter.api.Test;
import org.smojol.common.id.Identifiable;
import org.smojol.common.transpiler.FlowgraphTransformer;

import java.util.List;
import java.util.function.BiFunction;

public class IntervalAnalysisTest {
    @Test
    public void canMergeNodes() {
        Graph<TestNode, String> graph = new DefaultDirectedGraph<>(String.class);
        BiFunction<TestNode, TestNode, String> buildEdge = (v1, v2) -> String.format("(%s, %s)", v1, v2);
        FlowgraphTransformer<TestNode, String> transformer = new FlowgraphTransformer<>(graph, buildEdge);
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
        List<String> evolutions = transformer.reduce();
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
