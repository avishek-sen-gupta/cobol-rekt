package org.smojol.common.transpiler;

public class PlaceholderTranspilerNode extends LocationNode {
    private final String s;

    public PlaceholderTranspilerNode(String s) {
        this.s = s;
    }

    @Override
    public String description() {
        return "placeholder: " + s;
    }
}
