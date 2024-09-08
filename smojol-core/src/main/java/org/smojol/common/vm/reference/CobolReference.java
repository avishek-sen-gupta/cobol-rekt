package org.smojol.common.vm.reference;

import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.CobolDataType;
import org.smojol.common.vm.type.TypedRecord;

public interface CobolReference {
    TypedRecord resolveAs(CobolDataType type);
    CobolDataStructure resolve();
    CobolExpression asExpression();
    void set(CobolReference rhs);
}
