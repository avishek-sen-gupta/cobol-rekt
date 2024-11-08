package org.smojol.common.ast;

import com.google.gson.annotations.Expose;
import lombok.Getter;

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
}
