package com.mojo.algorithms.types;

import com.mojo.algorithms.domain.TypedRecord;

public record CobolDataType(UsageType usageType, TypedRecord defaultValue, AbstractCobolType abstractType) {
    public static final CobolDataType STRING = new CobolDataType(UsageType.DEFAULT, TypedRecord.typedString(""), AbstractCobolType.STRING);
    public static final CobolDataType BOOLEAN = new CobolDataType(UsageType.DEFAULT, TypedRecord.typedBoolean(false), AbstractCobolType.BOOLEAN);
    public static final CobolDataType ROOT = new CobolDataType(UsageType.DEFAULT, TypedRecord.NULL, AbstractCobolType.OBJECT);
    public static final CobolDataType GROUP = new CobolDataType(UsageType.DEFAULT, TypedRecord.NULL, AbstractCobolType.OBJECT);
    public static final CobolDataType NULL = new CobolDataType(UsageType.DEFAULT, TypedRecord.NULL, AbstractCobolType.NULL);
    public static final CobolDataType TABLE = new CobolDataType(UsageType.DEFAULT, TypedRecord.NULL, AbstractCobolType.OBJECT);
    public static final CobolDataType POINTER = new CobolDataType(UsageType.DEFAULT, TypedRecord.NULL, AbstractCobolType.POINTER);
    public static final CobolDataType NUMERIC_EXTERNAL_DECIMAL = new CobolDataType(UsageType.ZONED_DECIMAL, TypedRecord.NULL, AbstractCobolType.NUMBER);
    public static final CobolDataType COMPUTATIONAL3_DECIMAL = new CobolDataType(UsageType.COMPUTATIONAL3_DECIMAL, TypedRecord.NULL, AbstractCobolType.NUMBER);
}
