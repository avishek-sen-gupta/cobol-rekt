package org.smojol.common.transpiler;

public class ExitIterationScope extends LocationNode {
    @Override
    public String description() {
        return "break()";
    }
}
