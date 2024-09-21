package org.smojol.toolkit.analysis.defined;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.transpiler.FlowgraphTransformer;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.common.transpiler.TranspilerModel;
import org.smojol.toolkit.task.AnalysisTaskResult;

import java.util.List;

public class IntervalAnalysisTask {
    private final TranspilerModel model;

    public IntervalAnalysisTask(TranspilerModel model) {
        this.model = model;
    }

    public AnalysisTaskResult run() {
        Graph<TranspilerInstruction, DefaultEdge> jgraph = new DefaultDirectedGraph<>(DefaultEdge.class);
        model.instructions().forEach(jgraph::addVertex);
        model.instructionEdges().forEach(edge -> jgraph.addEdge(edge.from(), edge.to()));
        model.pruneUnreachables(jgraph);
        FlowgraphTransformer<TranspilerInstruction, DefaultEdge> transformer = new FlowgraphTransformer<>(jgraph, (a, b) -> new DefaultEdge());

        List<String> reductions = transformer.reduce();

        System.out.println(transformer.getEvolutions().getLast());
        System.out.println(transformer.isReducible());
        return AnalysisTaskResult.OK("INTERVAL_ANALYSIS", transformer);
    }
}
