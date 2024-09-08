package org.smojol.common.vm.expression;

import org.smojol.common.vm.structure.CobolDataStructure;

public interface ComparisonOperator {
    CobolExpression apply(CobolExpression lhs, CobolExpression rhs, CobolDataStructure structures);
    ComparisonOperator invert();
    String mnemonic();
}
