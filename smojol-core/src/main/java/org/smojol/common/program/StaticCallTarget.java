package org.smojol.common.program;

import org.smojol.common.ast.CallTarget;
import org.smojol.common.ast.ProgramReferenceType;

public class StaticCallTarget extends CallTarget {
    public StaticCallTarget(String callTarget) {
        super(callTarget, ProgramReferenceType.STATIC);
    }
}
