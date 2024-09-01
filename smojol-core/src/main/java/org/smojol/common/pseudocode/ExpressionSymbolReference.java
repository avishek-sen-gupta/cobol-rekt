package org.smojol.common.pseudocode;

import org.smojol.common.vm.expression.CobolExpression;

import java.util.Objects;

public class ExpressionSymbolReference extends SymbolReference {
    private final CobolExpression expression;

    public ExpressionSymbolReference(CobolExpression expression, String id) {
        super(id);
        this.expression = expression;
    }

    @Override
    public String toString() {
        return id;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ExpressionSymbolReference that = (ExpressionSymbolReference) o;
        return this.expression == that.expression;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(expression);
    }
}
