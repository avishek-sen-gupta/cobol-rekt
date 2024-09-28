package org.smojol.toolkit.analysis.defined;

import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.transpiler.FlowgraphReductionResult;
import org.smojol.common.transpiler.FlowgraphTransformer;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.common.transpiler.TranspilerModel;
import org.smojol.toolkit.task.AnalysisTaskResult;

import java.util.logging.Logger;

public class IntervalAnalysisTask {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(IntervalAnalysisTask.class.getName());
    private final TranspilerModel model;

    public IntervalAnalysisTask(TranspilerModel model) {
        this.model = model;
    }

    public AnalysisTaskResult run() {
        model.pruneUnreachables();
        FlowgraphTransformer<TranspilerInstruction, DefaultEdge> transformer = new FlowgraphTransformer<>(model.jgraph(), (a, b) -> new DefaultEdge(),
                instr -> FlowNodeType.PROCEDURE_DIVISION_BODY.equals(instr.ref().getProperty("type")));
        FlowgraphReductionResult<TranspilerInstruction, DefaultEdge> reductions = transformer.reduce();

        LOGGER.info(reductions.evolutions().getLast());
        LOGGER.info("" + reductions.isReducible());
        return AnalysisTaskResult.OK("INTERVAL_ANALYSIS", reductions);
    }
}
