package org.smojol.common.pseudocode;

import org.smojol.common.vm.expression.CobolExpression;

import java.util.Objects;
import java.util.UUID;

public class IntermediateSymbolReference extends SymbolReference {
    private final CobolExpression expression;

    public IntermediateSymbolReference(CobolExpression expression, String id) {
        super(id);
        this.expression = expression;
    }

    @Override
    public String toString() {
        return id;
    }

    public IntermediateSymbolReference() {
        super(UUID.randomUUID().toString());
        this.expression = null;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        IntermediateSymbolReference that = (IntermediateSymbolReference) o;
        return this.expression == that.expression;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(expression);
    }
}
