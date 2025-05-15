package com.mojo.algorithms.task;

import com.google.common.collect.ImmutableSet;
import com.mojo.algorithms.domain.CodeSentinelType;
import com.mojo.algorithms.transpiler.LabelledTranspilerCodeBlockNode;
import com.mojo.algorithms.domain.ProcedureRange;
import com.mojo.algorithms.domain.TranspilerInstruction;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.AsSubgraph;
import org.jgrapht.graph.DefaultEdge;

import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RangeBodyTask {
    private final Graph<TranspilerInstruction, DefaultEdge> flowgraph;

    public RangeBodyTask(Graph<TranspilerInstruction, DefaultEdge> flowgraph) {
        this.flowgraph = flowgraph;
    }

    public ProcedureRange run(String start, String end) {
        TranspilerInstruction rangeEntryVertex = flowgraph.vertexSet().stream().filter(v -> v.ref() instanceof LabelledTranspilerCodeBlockNode l
                && l.getName().equals(start)
                && v.sentinel() == CodeSentinelType.ENTER).findFirst().get();
        TranspilerInstruction rangeExitVertex = flowgraph.vertexSet().stream().filter(v -> v.ref() instanceof LabelledTranspilerCodeBlockNode l
                && l.getName().equals(end)
                && v.sentinel() == CodeSentinelType.EXIT).findFirst().get();
        return run(ImmutablePair.of(rangeEntryVertex, rangeExitVertex));
    }

    public ProcedureRange run(Pair<TranspilerInstruction, TranspilerInstruction> range) {
        return new ProcedureRange(range.getLeft(), range.getRight(), new AsSubgraph<>(flowgraph, trace(range.getLeft(), range.getRight(), ImmutableSet.of())));
    }

    private Set<TranspilerInstruction> trace(TranspilerInstruction current, TranspilerInstruction rangeExitVertex, Set<TranspilerInstruction> visited) {
        if (visited.contains(current)) return ImmutableSet.of();
        if (current == rangeExitVertex) return ImmutableSet.of(current);
        Set<DefaultEdge> outgoing = flowgraph.outgoingEdgesOf(current);
        return Stream.concat(Stream.of(current), outgoing.stream()
                        .map(flowgraph::getEdgeTarget)
                        .flatMap(o -> trace(o, rangeExitVertex,
                                Stream.concat(visited.stream(), Stream.of(current))
                                        .collect(Collectors.toUnmodifiableSet())).stream()))
                .collect(Collectors.toSet());
    }
}
