package org.smojol.ast;

public class StaticCallTarget extends CallTarget {
    public StaticCallTarget(String callTarget) {
        super(callTarget, ReferenceType.STATIC);
    }
}
