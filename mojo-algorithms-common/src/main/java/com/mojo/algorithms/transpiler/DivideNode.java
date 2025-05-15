package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerNode;
import lombok.Getter;
import com.mojo.algorithms.domain.SemanticCategory;

import java.util.Collection;

@Getter
public class DivideNode extends TranspilerNode {
    private final TranspilerNode dividend;
    private final TranspilerNode divisor;

    public DivideNode(TranspilerNode dividend, TranspilerNode divisor) {
        super(ImmutableList.of(SemanticCategory.COMPUTATIONAL, SemanticCategory.DATA_FLOW));
        this.dividend = dividend;
        this.divisor = divisor;
    }

    @Override
    public String description() {
        return String.format("divide(%s, %s)", dividend.description(), divisor.description());
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return ImmutableList.of(divisor, dividend);
    }
}
