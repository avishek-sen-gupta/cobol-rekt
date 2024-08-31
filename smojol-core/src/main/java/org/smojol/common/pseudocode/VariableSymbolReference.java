package org.smojol.common.pseudocode;

import org.smojol.common.vm.expression.CobolExpression;

import java.util.Objects;
import java.util.UUID;

public class VariableSymbolReference implements SymbolReference {
    private String id;
    private final CobolExpression expression;

    public VariableSymbolReference(CobolExpression expression, String id) {
        this.expression = expression;
        this.id = id;
    }

    @Override
    public String toString() {
        return id;
    }

    public VariableSymbolReference() {
        id = UUID.randomUUID().toString();
        this.expression = null;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        VariableSymbolReference that = (VariableSymbolReference) o;
        return this.expression == that.expression;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(expression);
    }
}
