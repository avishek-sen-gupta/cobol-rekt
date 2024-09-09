package org.smojol.common.vm.reference;

import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.VariableExpression;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ConversionStrategy;
import org.smojol.common.vm.type.AbstractCobolType;
import org.smojol.common.vm.type.TypedRecord;

public class VariableCobolReference implements CobolReference {
    private final CobolDataStructure dataStructure;

    public VariableCobolReference(CobolDataStructure dataStructure) {
        this.dataStructure = dataStructure;
    }

    @Override
    public TypedRecord resolveAs(AbstractCobolType type) {
        return ConversionStrategy.convert(dataStructure, type);
    }

    @Override
    public CobolDataStructure resolve() {
        return dataStructure;
    }

    @Override
    public CobolExpression asExpression() {
        return new VariableExpression(dataStructure.name());
    }

    @Override
    public void set(CobolReference rhs) {
        dataStructure.set(rhs);
    }
}
