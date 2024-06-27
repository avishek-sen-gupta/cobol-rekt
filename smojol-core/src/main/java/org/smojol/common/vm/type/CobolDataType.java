package org.smojol.common.vm.type;

public enum CobolDataType {
    STRING {
        @Override
        public TypedRecord defaultValue() {
            return TypedRecord.typedString("");
        }
    }, NUMBER {
        @Override
        public TypedRecord defaultValue() {
            return TypedRecord.typedNumber(0.0);
        }
    }, CONSTRAINT {
        @Override
        public TypedRecord defaultValue() {
            throw new IllegalArgumentException("Constraint does not have default value");
        }
    }, ROOT {
        @Override
        public TypedRecord defaultValue() {
            throw new IllegalArgumentException("Root does not have default value");
        }
    }, BOOLEAN {
        @Override
        public TypedRecord defaultValue() {
            return TypedRecord.FALSE;
        }
    }, GROUP {
        @Override
        public TypedRecord defaultValue() {
            throw new IllegalArgumentException("Group does not have default value");
        }
    }, NULL {
        @Override
        public TypedRecord defaultValue() {
            return TypedRecord.NULL;
        }
    }, TABLE {
        @Override
        public TypedRecord defaultValue() {
            return TypedRecord.NULL;
        }
    }, DETACHED {
        @Override
        public TypedRecord defaultValue() {
            return TypedRecord.typedNumber(0);
        }
    };

    public abstract TypedRecord defaultValue();
}
