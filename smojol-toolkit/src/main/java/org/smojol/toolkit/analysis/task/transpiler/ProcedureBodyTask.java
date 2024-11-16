package org.smojol.toolkit.analysis.task.transpiler;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.transpiler.*;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class ProcedureBodyTask {
    public Set<Pair<ProcedureRange, Set<ProcedureRange>>> run(TranspilerCodeBlockNode program, List<TranspilerInstruction> instructions, Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph) {
        Set<ProcedureRange> bodiesWithoutChildren = new CallRangesTask(program, instructions).run().stream().map(range -> new RangeBodyTask(instructionFlowgraph).run(range)).collect(Collectors.toUnmodifiableSet());
        return rangesWithChildren(bodiesWithoutChildren);
    }

    // TODO: Refactor this!!!
    public Set<Pair<ProcedureRange, Set<ProcedureRange>>> rangesWithChildren(Set<ProcedureRange> procRanges) {
        return procRanges.stream()
                .map(procRange -> {
                            Set<ProcedureRange> childRanges = procRange.body().vertexSet().stream()
                                    .filter(v -> v.ref() instanceof JumpTranspilerNode j && j.getEnd() != LocationNode.NULL)
                                    .flatMap(jmp -> {
                                        JumpTranspilerNode jmpNode = (JumpTranspilerNode) jmp.ref();
                                        return procRanges.stream().filter(bwc2 -> ((LabelledTranspilerCodeBlockNode) bwc2.entry().ref()).getName().equals(jmpNode.getStart().name())
                                                && ((LabelledTranspilerCodeBlockNode) bwc2.exit().ref()).getName().equals(jmpNode.getEnd().name()));
                                    }).collect(Collectors.toUnmodifiableSet());
                            return ImmutablePair.of(procRange, childRanges);
                        }
                ).collect(Collectors.toUnmodifiableSet());
    }
}
