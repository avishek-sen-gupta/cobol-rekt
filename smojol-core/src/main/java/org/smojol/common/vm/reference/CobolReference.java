package org.smojol.common.vm.reference;

import com.mojo.algorithms.domain.TypedRecord;
import com.mojo.algorithms.types.AbstractCobolType;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.structure.CobolDataStructure;

public interface CobolReference {
    TypedRecord resolveAs(AbstractCobolType type);
    CobolDataStructure resolve();
    CobolExpression asExpression();
    void set(CobolReference rhs);
}
