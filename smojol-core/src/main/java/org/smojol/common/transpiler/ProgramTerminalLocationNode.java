package org.smojol.common.transpiler;

public class ProgramTerminalLocationNode extends LocationNode {
    public static LocationNode END = new ProgramTerminalLocationNode();
    @Override
    public String description() {
        return "programEnd()";
    }
}
