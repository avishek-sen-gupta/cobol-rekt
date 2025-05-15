package org.smojol.toolkit.task;

import com.mojo.algorithms.domain.CodeSentinelType;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.domain.FlowgraphReductionResult;
import com.mojo.algorithms.domain.FlowgraphTransformer;
import com.mojo.algorithms.domain.TranspilerInstruction;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import com.mojo.algorithms.domain.FlowNodeType;
import com.mojo.algorithms.id.Identifiable;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.logging.Logger;

public class T1_T2_IntervalAnalysisTask<V extends Identifiable, E> {
    public static BiFunction<TranspilerInstruction, TranspilerInstruction, DefaultEdge> NEW_DEFAULT_EDGE = (a, b) -> new DefaultEdge();
    public static Function<TranspilerInstruction, Boolean> IS_ROOT = instr -> FlowNodeType.PROCEDURE_DIVISION_BODY.equals(instr.ref().getProperty("type")) && instr.sentinel() == CodeSentinelType.ENTER;

    private static final java.util.logging.Logger LOGGER = Logger.getLogger(T1_T2_IntervalAnalysisTask.class.getName());
    private final Graph<V, E> graph;
    private final Function<V, Boolean> isRoot;
    private final BiFunction<V, V, E> buildEdge;

    public T1_T2_IntervalAnalysisTask(Graph<V, E> graph, Function<V, Boolean> isRoot, BiFunction<V, V, E> buildEdge) {
        this.graph = graph;
        this.isRoot = isRoot;
        this.buildEdge = buildEdge;
    }

    public AnalysisTaskResult run() {
        FlowgraphTransformer<V, E> transformer = new FlowgraphTransformer<>(graph, buildEdge, isRoot);
        FlowgraphReductionResult<V, E> reductions = transformer.reduce();

        LOGGER.info(reductions.evolutions().getLast());
        LOGGER.info("" + reductions.isReducible());
        return AnalysisTaskResult.OK("INTERVAL_ANALYSIS", reductions);
    }
}
