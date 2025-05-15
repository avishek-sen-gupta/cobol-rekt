package com.mojo.algorithms.types;


import com.mojo.algorithms.domain.TypedRecord;

import static com.mojo.algorithms.domain.TypedRecord.typedBoolean;

public class ValueBasedComparator {
    public TypedRecord greaterThan(Object lhs, Object rhs, CobolDataType dataType) {
        if (dataType == CobolDataType.STRING) return typedBoolean(((String) lhs).compareTo((String) rhs) > 0);
        return typedBoolean((Double) lhs > (Double) rhs);
    }

    public TypedRecord greaterThanOrEqualTo(Object lhs, Object rhs, CobolDataType dataType) {
        if (dataType == CobolDataType.STRING) return typedBoolean(((String) lhs).compareTo((String) rhs) >= 0);
        return typedBoolean((Double) lhs >= (Double) rhs);
    }

    public TypedRecord equalTo(Object lhs, Object rhs, CobolDataType dataType) {
//        if (dataType == CobolDataType.STRING) return typedBoolean(((String) lhs).compareTo((String) rhs) == 0);
        return typedBoolean(lhs.equals(rhs));
    }

    public TypedRecord lessThan(Object lhs, Object rhs, CobolDataType dataType) {
        if (dataType == CobolDataType.STRING) return typedBoolean(((String) lhs).compareTo((String) rhs) < 0);
        return typedBoolean((Double) lhs < (Double) rhs);
    }

    public TypedRecord lessThanOrEqualTo(Object lhs, Object rhs, CobolDataType dataType) {
        if (dataType == CobolDataType.STRING) return typedBoolean(((String) lhs).compareTo((String) rhs) <= 0);
        return typedBoolean((Double) lhs <= (Double) rhs);
    }
}
