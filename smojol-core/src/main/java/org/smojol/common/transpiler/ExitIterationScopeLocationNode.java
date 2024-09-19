package org.smojol.common.transpiler;

public class ExitIterationScopeLocationNode extends LocationNode {
    @Override
    public String description() {
        return "break()";
    }
}
