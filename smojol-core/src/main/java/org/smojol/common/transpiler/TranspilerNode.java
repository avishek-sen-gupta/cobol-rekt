package org.smojol.common.transpiler;

public abstract class TranspilerNode {
    @Override
    public String toString() {
        return description();
    }

    public abstract String description();
}
