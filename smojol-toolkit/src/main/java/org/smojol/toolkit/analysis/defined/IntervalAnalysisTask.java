package org.smojol.toolkit.analysis.defined;

import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.transpiler.FlowgraphReductionResult;
import org.smojol.common.transpiler.FlowgraphTransformer;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.common.transpiler.TranspilerModel;
import org.smojol.toolkit.task.AnalysisTaskResult;

public class IntervalAnalysisTask {
    private final TranspilerModel model;

    public IntervalAnalysisTask(TranspilerModel model) {
        this.model = model;
    }

    public AnalysisTaskResult run() {
        model.pruneUnreachables();
        FlowgraphTransformer<TranspilerInstruction, DefaultEdge> transformer = new FlowgraphTransformer<>(model.jgraph(), (a, b) -> new DefaultEdge());
        FlowgraphReductionResult<TranspilerInstruction, DefaultEdge> reductions = transformer.reduce();

        System.out.println(reductions.evolutions().getLast());
        System.out.println(reductions.isReducible());
        return AnalysisTaskResult.OK("INTERVAL_ANALYSIS", reductions);
    }
}
