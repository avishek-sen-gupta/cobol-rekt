package org.smojol.common.vm.reference;

import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ConversionStrategy;
import org.smojol.common.vm.type.CobolDataType;
import org.smojol.common.vm.type.TypedRecord;

public class PrimitiveReference implements CobolReference {
    private final TypedRecord value;
    public PrimitiveReference(TypedRecord value) {
        this.value = value;
    }

    @Override
    public TypedRecord resolveAs(CobolDataType type) {
        return ConversionStrategy.convert(value, type);
    }

    @Override
    public CobolDataStructure resolve() {
        return new DetachedDataStructure(value);
    }
}
