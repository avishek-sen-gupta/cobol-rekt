package org.smojol.common.vm.expression;

public class RelationalOperation {
    public static final ComparisonOperator EQUAL = CobolExpression::equalTo;
    public static final ComparisonOperator NOT_EQUAL = (lhs, rhs, d) -> lhs.equalTo(rhs, d).not(d);
    public static final ComparisonOperator LESS_THAN = CobolExpression::lessThan;
    public static final ComparisonOperator GREATER_THAN = CobolExpression::greaterThan;
    public static final ComparisonOperator LESS_THAN_OR_EQUAL = CobolExpression::lessThanOrEqualTo;
    public static final ComparisonOperator GREATER_THAN_OR_EQUAL = CobolExpression::greaterThanOrEqualTo;
}
