package org.smojol.common.pseudocode;

import java.util.Objects;

public class IntermediateSymbolReference extends SymbolReference {
    public IntermediateSymbolReference(String id) {
        super(id);
    }

    @Override
    public String toString() {
        return id;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        IntermediateSymbolReference that = (IntermediateSymbolReference) o;
        return this.id.equals(that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
