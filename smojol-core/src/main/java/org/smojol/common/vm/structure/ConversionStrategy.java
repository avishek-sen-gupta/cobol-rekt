package org.smojol.common.vm.structure;

import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.type.AbstractCobolType;
import org.smojol.common.vm.type.TypedRecord;

public class ConversionStrategy {
    public static void set(CobolDataStructure lhs, CobolReference rhs) {
        TypedRecord typedRecord = rhs.resolveAs(AbstractCobolType.STRING);
        lhs.internalSet(typedRecord);
    }

    public static TypedRecord convert(TypedRecord record, AbstractCobolType targetDataType) {
        return switch (targetDataType) {
            case STRING -> TypedRecord.typedString(record.value().toString());
            case NUMBER -> TypedRecord.typedNumber(Double.parseDouble(record.value().toString()));
            case BOOLEAN -> TypedRecord.typedBoolean(Boolean.parseBoolean(record.value().toString()));
            default -> throw new ClassCastException(String.format("Cannot cast %s to requested type: %s", record, targetDataType));
        };
    }

    // structure either contains substructures or a TypedRecord
    public static TypedRecord convert(CobolDataStructure structure, AbstractCobolType type) {
        return structure.getValue();
    }

    public static Double asNumber(String s) {
        if (s.trim().isEmpty()) return 0.;
        return Double.parseDouble(s);
    }

    public static String asString(String s) {
        return s.replace("'", "");
    }
}
