package com.mojo.algorithms.transpiler;

import java.util.Set;

public record InvokingProcedureRange(ProcedureRange range,
                                     Set<ProcedureRange> invokedRanges) {
    @Override
    public String toString() {
        return range.toString() + " /// " + String.join(",", invokedRanges.stream().map(ProcedureRange::toString).toList());
    }
}
