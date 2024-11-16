package org.smojol.common.transpiler;

import lombok.Getter;
import org.smojol.common.pseudocode.CodeSentinelType;

@Getter
public class IdLocationNode extends LocationNode {
    private final TranspilerNode destination;
    private final CodeSentinelType sentinel;

    public IdLocationNode(TranspilerNode destination, CodeSentinelType sentinel) {
        this.destination = destination;
        this.sentinel = sentinel;
    }

    @Override
    public String description() {
        return String.format("loc(id=%s)", destination.id());
    }

    @Override
    public String name() {
        return destination.id();
    }
}
