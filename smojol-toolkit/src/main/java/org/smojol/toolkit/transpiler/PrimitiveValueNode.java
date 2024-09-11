package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.type.TypedRecord;

public class PrimitiveValueNode implements TranspilerNode {
    private final TypedRecord value;

    public PrimitiveValueNode(TypedRecord value) {
        this.value = value;
    }
}
