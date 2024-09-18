package org.smojol.common.transpiler;

import lombok.Getter;

public class NamedLocationNode extends LocationNode {
    @Getter private final String name;

    public NamedLocationNode(String name) {
        this.name = name;
    }

    @Override
    public String description() {
        return String.format("loc(%s)", name);
    }
}
