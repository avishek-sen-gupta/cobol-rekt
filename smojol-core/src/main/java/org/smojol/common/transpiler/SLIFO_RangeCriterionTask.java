package org.smojol.common.transpiler;

import com.google.common.collect.Sets;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;

import java.util.Set;
import java.util.stream.Collectors;

public class SLIFO_RangeCriterionTask {
    private final Set<Pair<Pair<TranspilerInstruction, TranspilerInstruction>, Graph<TranspilerInstruction, DefaultEdge>>> rangeBodies;

    public SLIFO_RangeCriterionTask(Set<Pair<Pair<TranspilerInstruction, TranspilerInstruction>, Graph<TranspilerInstruction, DefaultEdge>>> rangeBodies) {
        this.rangeBodies = rangeBodies;
    }

    public Set<Pair<Pair<TranspilerInstruction, TranspilerInstruction>, Graph<TranspilerInstruction, DefaultEdge>>> rangesTerminatingIn(Pair<Pair<TranspilerInstruction, TranspilerInstruction>, Graph<TranspilerInstruction, DefaultEdge>> range) {
        Set<TranspilerInstruction> body = range.getRight().vertexSet();
        Set<Pair<Pair<TranspilerInstruction, TranspilerInstruction>, Graph<TranspilerInstruction, DefaultEdge>>> rangesTerminatingInCurrentRangeIncludingSelf = rangeBodies.stream()
                .filter(bbr -> body.contains(bbr.getLeft().getRight()))
                .collect(Collectors.toUnmodifiableSet());
        return Sets.difference(rangesTerminatingInCurrentRangeIncludingSelf, Set.of(range));
    }
}
