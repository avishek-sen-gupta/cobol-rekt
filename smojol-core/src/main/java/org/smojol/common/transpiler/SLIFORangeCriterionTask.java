package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;

import java.util.Set;
import java.util.stream.Collectors;

public class SLIFORangeCriterionTask {
    private final Set<InvokingProcedureRange> rangesWithChildren;

    public SLIFORangeCriterionTask(Set<InvokingProcedureRange> rangesWithChildren) {
        this.rangesWithChildren = rangesWithChildren;
    }

    public Set<ProcedureRange> rangesTerminatingIn(ProcedureRange range) {
        Set<ProcedureRange> procedureRanges = rangesWithChildren.stream().map(InvokingProcedureRange::range).collect(Collectors.toUnmodifiableSet());
        Set<TranspilerInstruction> bodyVertices = range.body().vertexSet();
        Set<TranspilerInstruction> selfExit = bodyVertices.stream().filter(v -> v == range.exit()).collect(Collectors.toUnmodifiableSet());
        Set<TranspilerInstruction> body = Sets.difference(bodyVertices, selfExit);
//        Set<TranspilerInstruction> body = bodyVertices;
        Set<ProcedureRange> rangesTerminatingInCurrentRangeIncludingSelf = procedureRanges.stream()
                .filter(bbr -> body.contains(bbr.exit()))
                .collect(Collectors.toUnmodifiableSet());
        return Sets.difference(rangesTerminatingInCurrentRangeIncludingSelf, Set.of(range));
    }

    public boolean isSLIFO(InvokingProcedureRange rangeWithChildren, Set<ProcedureRange> existingSLIFORanges) {
        Set<ProcedureRange> rangesTerminatingInRange = rangesTerminatingIn(rangeWithChildren.range());
        Set<ProcedureRange> invokedRanges = rangeWithChildren.invokedRanges();
        return Sets.difference(rangesTerminatingInRange, existingSLIFORanges).isEmpty()
                && Sets.difference(invokedRanges, existingSLIFORanges).isEmpty();
    }

    public Set<InvokingProcedureRange> allSLIFORanges(Set<InvokingProcedureRange> rangesWithChildren, Set<InvokingProcedureRange> existingSLIFORanges) {
        Set<InvokingProcedureRange> newlyDiscoveredSLIFOProcedures = rangesWithChildren.stream()
                .filter(rwc -> isSLIFO(rwc, existingSLIFORanges.stream()
                        .map(InvokingProcedureRange::range)
                        .collect(Collectors.toUnmodifiableSet())))
                .collect(Collectors.toUnmodifiableSet());
        if (newlyDiscoveredSLIFOProcedures.isEmpty()) return ImmutableSet.of();
        return Sets.union(newlyDiscoveredSLIFOProcedures, allSLIFORanges(
                Sets.difference(rangesWithChildren, newlyDiscoveredSLIFOProcedures),
                Sets.union(existingSLIFORanges, newlyDiscoveredSLIFOProcedures)));
    }

    public Set<InvokingProcedureRange> allSLIFORanges(Set<InvokingProcedureRange> rangesWithChildren) {
        return allSLIFORanges(rangesWithChildren, ImmutableSet.of());
    }
}
