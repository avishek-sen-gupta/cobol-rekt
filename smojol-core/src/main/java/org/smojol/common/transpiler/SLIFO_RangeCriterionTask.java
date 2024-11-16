package org.smojol.common.transpiler;

import com.google.common.collect.Sets;

import java.util.Set;
import java.util.stream.Collectors;

public class SLIFO_RangeCriterionTask {
    private final Set<ProcedureRange> rangeBodies;

    public SLIFO_RangeCriterionTask(Set<ProcedureRange> rangeBodies) {
        this.rangeBodies = rangeBodies;
    }

    public Set<ProcedureRange> rangesTerminatingIn(ProcedureRange range) {
        Set<TranspilerInstruction> body = range.body().vertexSet();
        Set<ProcedureRange> rangesTerminatingInCurrentRangeIncludingSelf = rangeBodies.stream()
                .filter(bbr -> body.contains(bbr.exit()))
                .collect(Collectors.toUnmodifiableSet());
        return Sets.difference(rangesTerminatingInCurrentRangeIncludingSelf, Set.of(range));
    }
}
