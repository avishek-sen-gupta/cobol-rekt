package org.smojol.common.transpiler;

public class ListIterationNode extends TranspilerNode {
    private final TranspilerNode iterable;
    private final TranspilerNode body;

    public ListIterationNode(TranspilerNode iterable, TranspilerNode body) {
        this.iterable = iterable;
        this.body = body;
    }

    @Override
    public String description() {
        return String.format("iterate(%s) {\n%s\n}", iterable.description(), body.description());
    }
}
