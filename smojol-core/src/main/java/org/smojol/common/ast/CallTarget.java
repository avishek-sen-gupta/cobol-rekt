package org.smojol.common.ast;

import com.google.gson.annotations.Expose;
import lombok.Getter;
import org.smojol.common.pseudocode.PseudocodeInstruction;

import java.util.List;

public abstract class CallTarget {
    @Expose @Getter private final String name;
    @Expose @Getter private final ProgramReferenceType programReferenceType;

    public CallTarget(String callTargetString, ProgramReferenceType programReferenceType) {
        this.name = callTargetString.trim().replace("\"", "").replace("'", "");
        this.programReferenceType = programReferenceType;
    }

    @Override
    public String toString() {
        return name;
    }

    public abstract CallTarget resolve(PseudocodeInstruction instruction, List<PseudocodeInstruction> instructions);
}
