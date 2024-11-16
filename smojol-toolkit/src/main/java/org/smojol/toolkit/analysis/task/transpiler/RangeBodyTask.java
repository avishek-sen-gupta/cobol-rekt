package org.smojol.toolkit.analysis.task.transpiler;

import com.google.common.collect.ImmutableSet;
import org.jgrapht.Graph;
import org.jgrapht.graph.AsSubgraph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.pseudocode.CodeSentinelType;
import org.smojol.common.transpiler.JumpTranspilerNode;
import org.smojol.common.transpiler.LabelledTranspilerCodeBlockNode;
import org.smojol.common.transpiler.LocationNode;
import org.smojol.common.transpiler.TranspilerInstruction;

import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RangeBodyTask {
    private final Graph<TranspilerInstruction, DefaultEdge> flowgraph;

    public RangeBodyTask(Graph<TranspilerInstruction, DefaultEdge> flowgraph) {
        this.flowgraph = flowgraph;
    }

    public Graph<TranspilerInstruction, DefaultEdge> run(String start, String end) {
        TranspilerInstruction rangeEntryVertex = flowgraph.vertexSet().stream().filter(v -> v.ref() instanceof LabelledTranspilerCodeBlockNode l
                && l.getName().equals(start)
                && v.sentinel() == CodeSentinelType.ENTER).findFirst().get();
        TranspilerInstruction rangeExitVertex = flowgraph.vertexSet().stream().filter(v -> v.ref() instanceof LabelledTranspilerCodeBlockNode l
                && l.getName().equals(end)
                && v.sentinel() == CodeSentinelType.EXIT).findFirst().get();
        return new AsSubgraph<>(flowgraph, trace(rangeEntryVertex, rangeExitVertex, ImmutableSet.of()));
    }

    private Set<TranspilerInstruction> trace(TranspilerInstruction current, TranspilerInstruction rangeExitVertex, Set<TranspilerInstruction> visited) {
        if (visited.contains(current)) return ImmutableSet.of();
        if (current == rangeExitVertex) return ImmutableSet.of(current);
        Set<DefaultEdge> outgoing = flowgraph.outgoingEdgesOf(current);
        return Stream.concat(Stream.of(current), outgoing.stream()
                        .map(flowgraph::getEdgeTarget)
                        .filter(this::isNotProcedureCall)
                        .flatMap(o -> trace(o, rangeExitVertex,
                                Stream.concat(visited.stream(), Stream.of(current))
                                        .collect(Collectors.toUnmodifiableSet())).stream()))
                .collect(Collectors.toSet());
    }

    private boolean isNotProcedureCall(TranspilerInstruction transpilerInstruction) {
        return !(transpilerInstruction.ref() instanceof JumpTranspilerNode)
                || (transpilerInstruction.ref() instanceof JumpTranspilerNode j
                && j.getEnd() == LocationNode.NULL);
    }
}
