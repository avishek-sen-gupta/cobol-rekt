package org.smojol.common.vm.expression;

import org.smojol.common.vm.structure.CobolDataStructure;

public class RelationalOperation {
    public static final ComparisonOperator EQUAL = new ComparisonOperator() {
        @Override
        public CobolExpression apply(CobolExpression lhs, CobolExpression rhs, CobolDataStructure structures) {
            return lhs.equalTo(rhs, structures);
        }

        @Override
        public ComparisonOperator invert() {
            return NOT_EQUAL;
        }

    @Override
    public String mnemonic() {
        return "=";
    }
};

    public static final ComparisonOperator NOT_EQUAL = new ComparisonOperator() {
        @Override
        public CobolExpression apply(CobolExpression lhs, CobolExpression rhs, CobolDataStructure structures) {
            return lhs.equalTo(rhs, structures).not(structures);
        }

        @Override
        public ComparisonOperator invert() {
            return EQUAL;
        }

        @Override
        public String mnemonic() {
            return "<>";
        }
    };

    public static final ComparisonOperator LESS_THAN = new ComparisonOperator() {
        @Override
        public CobolExpression apply(CobolExpression lhs, CobolExpression rhs, CobolDataStructure structures) {
            return lhs.lessThan(rhs, structures);
        }

        @Override
        public ComparisonOperator invert() {
            return GREATER_THAN_OR_EQUAL;
        }

        @Override
        public String mnemonic() {
            return "<";
        }
    };
    public static final ComparisonOperator GREATER_THAN = new ComparisonOperator() {
        @Override
        public CobolExpression apply(CobolExpression lhs, CobolExpression rhs, CobolDataStructure structures) {
            return lhs.greaterThan(rhs, structures);
        }

        @Override
        public ComparisonOperator invert() {
            return LESS_THAN_OR_EQUAL;
        }

        @Override
        public String mnemonic() {
            return ">";
        }
    };

    public static final ComparisonOperator LESS_THAN_OR_EQUAL = new ComparisonOperator() {
        @Override
        public CobolExpression apply(CobolExpression lhs, CobolExpression rhs, CobolDataStructure structures) {
            return lhs.lessThanOrEqualTo(rhs, structures);
        }

        @Override
        public ComparisonOperator invert() {
            return GREATER_THAN;
        }

        @Override
        public String mnemonic() {
            return "<=";
        }
    };

    public static final ComparisonOperator GREATER_THAN_OR_EQUAL = new ComparisonOperator() {
        @Override
        public CobolExpression apply(CobolExpression lhs, CobolExpression rhs, CobolDataStructure structures) {
            return lhs.greaterThanOrEqualTo(rhs, structures);
        }

        @Override
        public ComparisonOperator invert() {
            return LESS_THAN;
        }

        @Override
        public String mnemonic() {
            return ">=";
        }
    };
}
