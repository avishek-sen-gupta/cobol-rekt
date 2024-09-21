package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;
import org.smojol.common.vm.type.TypedRecord;

public class PrimitiveValueTranspilerNode extends TranspilerNode {
    private final TypedRecord value;

    public PrimitiveValueTranspilerNode(TypedRecord value) {
        super(ImmutableList.of(SemanticCategory.REFERENCE));
        this.value = value;
    }

    @Override
    public String description() {
        return String.format("primitive(%s)", value.value());
    }
}
