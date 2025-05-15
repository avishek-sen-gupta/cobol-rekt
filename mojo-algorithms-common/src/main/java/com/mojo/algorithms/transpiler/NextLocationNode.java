package com.mojo.algorithms.transpiler;

public class NextLocationNode extends LocationNode {
    @Override
    public String description() {
        return "next()";
    }

    @Override
    public String name() {
        return "continue";
    }
}
