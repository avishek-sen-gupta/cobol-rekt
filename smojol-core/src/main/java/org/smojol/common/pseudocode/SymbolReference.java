package org.smojol.common.pseudocode;

public abstract class SymbolReference {
    protected String id;

    public SymbolReference(String id) {
        this.id = id;
    }

    public String id() {
        return id;
    }
}
