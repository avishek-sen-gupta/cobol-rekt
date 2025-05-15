package com.mojo.algorithms.task;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;
import com.mojo.algorithms.domain.InvokingProcedureRange;
import com.mojo.algorithms.domain.ProcedureRange;
import com.mojo.algorithms.domain.TranspilerInstruction;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import java.util.Set;
import java.util.stream.Collectors;

public class SLIFORangeCriterionTask {
    private final Set<InvokingProcedureRange> rangesWithChildren;

    public SLIFORangeCriterionTask(Set<InvokingProcedureRange> rangesWithChildren) {
        this.rangesWithChildren = rangesWithChildren;
    }

    public Pair<Set<InvokingProcedureRange>, Set<InvokingProcedureRange>> run() {
        Set<InvokingProcedureRange> allSLIFORanges = allSLIFORanges(this.rangesWithChildren, ImmutableSet.of());
        Set<InvokingProcedureRange> nonSLIFORanges = Sets.difference(rangesWithChildren, allSLIFORanges);
        return ImmutablePair.of(allSLIFORanges, nonSLIFORanges);
    }

    public Set<ProcedureRange> rangesTerminatingIn(ProcedureRange range) {
        Set<ProcedureRange> procedureRanges = rangesWithChildren.stream().map(InvokingProcedureRange::range).collect(Collectors.toUnmodifiableSet());
        Set<TranspilerInstruction> bodyVertices = range.body().vertexSet();
        Set<TranspilerInstruction> selfExit = bodyVertices.stream().filter(v -> v == range.exit()).collect(Collectors.toUnmodifiableSet());
        Set<TranspilerInstruction> body = Sets.difference(bodyVertices, selfExit);
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

    private Set<InvokingProcedureRange> allSLIFORanges(Set<InvokingProcedureRange> uncategorisedRanges, Set<InvokingProcedureRange> existingSLIFORanges) {
        Set<InvokingProcedureRange> newlyDiscoveredSLIFOProcedures = uncategorisedRanges.stream()
                .filter(rwc -> isSLIFO(rwc, existingSLIFORanges.stream()
                        .map(InvokingProcedureRange::range)
                        .collect(Collectors.toUnmodifiableSet())))
                .collect(Collectors.toUnmodifiableSet());
        if (newlyDiscoveredSLIFOProcedures.isEmpty()) return ImmutableSet.of();
        return Sets.union(newlyDiscoveredSLIFOProcedures, allSLIFORanges(
                Sets.difference(uncategorisedRanges, newlyDiscoveredSLIFOProcedures),
                Sets.union(existingSLIFORanges, newlyDiscoveredSLIFOProcedures)));
    }
}
