package org.smojol.common.transpiler;

public class NamedLocationNode extends LocationNode {
    private final String name;

    public NamedLocationNode(String name) {
        this.name = name;
    }

    @Override
    public String description() {
        return String.format("loc(%s)", name);
    }
}
