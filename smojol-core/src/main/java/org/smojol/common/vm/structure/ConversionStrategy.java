package org.smojol.common.vm.structure;

import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.PrimitiveCobolExpression;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.type.CobolDataType;
import org.smojol.common.vm.type.TypedRecord;

public class ConversionStrategy {
    public static void set(CobolDataStructure lhs, CobolReference rhs) {
        TypedRecord typedRecord = rhs.resolveAs(CobolDataType.STRING);
        lhs.internalSet(typedRecord);
    }


    public static TypedRecord convert(TypedRecord record, CobolDataType targetDataType) {
        return switch (targetDataType) {
            case STRING -> TypedRecord.typedString(record.value().toString());
            case NUMBER -> TypedRecord.typedNumber(Double.parseDouble(record.value().toString()));
            default -> throw new UnsupportedOperationException("Unsupported data type: " + targetDataType);
        };
    }

    // structure either contains substructures or a TypedRecord
    public static TypedRecord convert(CobolDataStructure structure, CobolDataType type) {
        return structure.getValue();
    }

    public static Double asNumber(String s) {
        if (s.trim().isEmpty()) return 0.;
        return Double.parseDouble(s);
    }

    public static String asString(String s) {
        return s.replace("'", "");
    }

    public static PrimitiveCobolExpression primitive(CobolExpression expression) {
        if (!(expression instanceof PrimitiveCobolExpression))
            throw new IncompatibleClassChangeError("Expression is of type " + expression.getClass() + ", not a primitive expression");
        return (PrimitiveCobolExpression) expression;
    }
}
