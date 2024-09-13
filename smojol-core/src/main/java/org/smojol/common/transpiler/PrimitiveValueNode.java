package org.smojol.common.transpiler;

import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.type.TypedRecord;

public class PrimitiveValueNode extends TranspilerNode {
    private final TypedRecord value;

    public PrimitiveValueNode(TypedRecord value) {
        this.value = value;
    }

    @Override
    public String description() {
        return String.format("primitive(%s)", value.value());
    }
}
