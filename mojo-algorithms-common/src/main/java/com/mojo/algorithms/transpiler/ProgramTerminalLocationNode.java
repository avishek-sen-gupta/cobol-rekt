package com.mojo.algorithms.transpiler;

public class ProgramTerminalLocationNode extends LocationNode {
    public static LocationNode END = new ProgramTerminalLocationNode();
    @Override
    public String description() {
        return "programEnd()";
    }

    @Override
    public String name() {
        return "TERM";
    }
}
