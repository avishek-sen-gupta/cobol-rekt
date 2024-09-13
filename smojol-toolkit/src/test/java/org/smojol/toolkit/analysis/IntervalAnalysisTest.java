package org.smojol.toolkit.analysis;

import org.jgrapht.Graph;
import org.jgrapht.graph.DirectedPseudograph;
import org.junit.jupiter.api.Test;
import org.smojol.common.transpiler.FlowgraphTransformer;

import java.util.function.BiFunction;

public class IntervalAnalysisTest {
    @Test
    public void canMergeNodes() {
        Graph<String, String> graph = new DirectedPseudograph<>(String.class);
        BiFunction<String, String, String> buildEdge = (v1, v2) -> v1 + v2;
        FlowgraphTransformer<String, String> transformer = new FlowgraphTransformer<>(buildEdge, graph);
        transformer.addVertex("1");
        transformer.addVertex("2");
        transformer.addVertex("3");
        transformer.addEdge("1", "2");
        transformer.addEdge("2", "3");
        transformer.addEdge("1", "1");
        transformer.reduce();
    }
}
