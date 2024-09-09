package org.smojol.common.vm.reference;

import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.PrimitiveCobolExpression;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ConversionStrategy;
import org.smojol.common.vm.type.AbstractCobolType;
import org.smojol.common.vm.type.TypedRecord;

public class PrimitiveReference implements CobolReference {
    private final TypedRecord value;
    public PrimitiveReference(TypedRecord value) {
        this.value = value;
    }

    @Override
    public TypedRecord resolveAs(AbstractCobolType type) {
        return ConversionStrategy.convert(value, type);
    }

    @Override
    public CobolDataStructure resolve() {
        return new DetachedDataStructure(value);
    }

    @Override
    public CobolExpression asExpression() {
        return new PrimitiveCobolExpression(value);
    }

    @Override
    public void set(CobolReference rhs) {
        throw new UnsupportedOperationException("Cannot reference primitive expressions");
    }
}
