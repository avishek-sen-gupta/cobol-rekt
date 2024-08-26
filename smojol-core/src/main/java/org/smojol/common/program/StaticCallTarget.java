package org.smojol.common.program;

import org.smojol.common.ast.CallTarget;
import org.smojol.common.ast.ProgramReferenceType;
import org.smojol.common.pseudocode.PseudocodeInstruction;

import java.util.List;

public class StaticCallTarget extends CallTarget {
    public StaticCallTarget(String callTarget) {
        super(callTarget, ProgramReferenceType.STATIC);
    }

    @Override
    public CallTarget resolve(PseudocodeInstruction instruction, List<PseudocodeInstruction> instructions) {
        return this;
    }
}
