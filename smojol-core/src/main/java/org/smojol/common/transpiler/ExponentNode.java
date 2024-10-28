package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.ast.SemanticCategory;

import java.util.Collection;

@Getter
public class ExponentNode extends TranspilerNode {
    private final TranspilerNode basis;
    private final TranspilerNode exponent;

    public ExponentNode(TranspilerNode basis, TranspilerNode exponent) {
        super(ImmutableList.of(SemanticCategory.COMPUTATIONAL));
        this.basis = basis;
        this.exponent = exponent;
    }

    @Override
    public String description() {
        return String.format("exp(%s, %s)", basis.description(), exponent.description());
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return ImmutableList.of(basis, exponent);
    }
}
