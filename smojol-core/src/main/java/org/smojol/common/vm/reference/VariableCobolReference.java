package org.smojol.common.vm.reference;

import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ConversionStrategy;
import org.smojol.common.vm.type.CobolDataType;
import org.smojol.common.vm.type.TypedRecord;

public class VariableCobolReference implements CobolReference {
    private final CobolDataStructure dataStructure;

    public VariableCobolReference(CobolDataStructure dataStructure) {
        this.dataStructure = dataStructure;
    }

    @Override
    public TypedRecord resolveAs(CobolDataType type) {
        return ConversionStrategy.convert(dataStructure, type);
    }

    @Override
    public CobolDataStructure resolve() {
        return dataStructure;
    }

    @Override
    public void set(CobolReference rhs) {
        dataStructure.set(rhs);
    }
}
