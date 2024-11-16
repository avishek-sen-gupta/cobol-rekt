package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;
import org.apache.commons.lang3.tuple.Pair;

import java.util.Set;
import java.util.stream.Collectors;

public class SLIFORangeCriterionTask {
    private final Set<Pair<ProcedureRange, Set<ProcedureRange>>> rangesWithChildren;

    public SLIFORangeCriterionTask(Set<Pair<ProcedureRange, Set<ProcedureRange>>> rangesWithChildren) {
        this.rangesWithChildren = rangesWithChildren;
    }

    public Set<ProcedureRange> rangesTerminatingIn(ProcedureRange range) {
        Set<ProcedureRange> procedureRanges = rangesWithChildren.stream().map(Pair::getLeft).collect(Collectors.toUnmodifiableSet());
        Set<TranspilerInstruction> body = range.body().vertexSet();
        Set<ProcedureRange> rangesTerminatingInCurrentRangeIncludingSelf = procedureRanges.stream()
                .filter(bbr -> body.contains(bbr.exit()))
                .collect(Collectors.toUnmodifiableSet());
        return Sets.difference(rangesTerminatingInCurrentRangeIncludingSelf, Set.of(range));
    }

    public boolean isSLIFO(Pair<ProcedureRange, Set<ProcedureRange>> rangeWithChildren, Set<ProcedureRange> existingSLIFORanges) {
        Set<ProcedureRange> rangesTerminatingInRange = rangesTerminatingIn(rangeWithChildren.getLeft());
        Set<ProcedureRange> invokedRanges = rangeWithChildren.getRight();
        return Sets.difference(rangesTerminatingInRange, existingSLIFORanges).isEmpty()
                && Sets.difference(invokedRanges, existingSLIFORanges).isEmpty();
    }

    public Set<Pair<ProcedureRange, Set<ProcedureRange>>> allSLIFORanges(Set<Pair<ProcedureRange, Set<ProcedureRange>>> rangesWithChildren, Set<Pair<ProcedureRange, Set<ProcedureRange>>> existingSLIFORanges) {
        Set<Pair<ProcedureRange, Set<ProcedureRange>>> newlyDiscoveredSLIFOProcedures = rangesWithChildren.stream()
                .filter(rwc -> isSLIFO(rwc, existingSLIFORanges.stream()
                        .map(Pair::getLeft)
                        .collect(Collectors.toUnmodifiableSet())))
                .collect(Collectors.toUnmodifiableSet());
        if (newlyDiscoveredSLIFOProcedures.isEmpty()) return ImmutableSet.of();
        return Sets.union(newlyDiscoveredSLIFOProcedures, allSLIFORanges(
                Sets.difference(rangesWithChildren, newlyDiscoveredSLIFOProcedures),
                Sets.union(existingSLIFORanges, newlyDiscoveredSLIFOProcedures)));
    }

    public Set<Pair<ProcedureRange, Set<ProcedureRange>>> allSLIFORanges(Set<Pair<ProcedureRange, Set<ProcedureRange>>> rangesWithChildren) {
        return allSLIFORanges(rangesWithChildren, ImmutableSet.of());
    }
}
