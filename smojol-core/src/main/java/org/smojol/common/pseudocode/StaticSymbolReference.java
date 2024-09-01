package org.smojol.common.pseudocode;

import org.smojol.common.vm.type.TypedRecord;

import java.util.Objects;

public class StaticSymbolReference extends SymbolReference {
    private final TypedRecord data;

    public StaticSymbolReference(TypedRecord data, String id) {
        super(id);
        this.data = data;
    }

    @Override
    public String toString() {
        return data.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        StaticSymbolReference that = (StaticSymbolReference) o;
        return Objects.equals(data, that.data);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(data);
    }
}
