package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerNode;
import lombok.Getter;
import com.mojo.algorithms.domain.SemanticCategory;

import java.util.Collection;

@Getter
public class ListIterationTranspilerNode extends TranspilerNode {
    private final TranspilerNode iterable;
    private final TranspilerNode body;

    public ListIterationTranspilerNode(TranspilerNode iterable, TranspilerNode body) {
        super(ImmutableList.of(body), ImmutableList.of(SemanticCategory.ITERATION, SemanticCategory.REFERENCE));
        this.iterable = iterable;
        this.body = body;
    }

    @Override
    public String description() {
        return String.format("iterate(%s) {\n%s\n}", iterable.description(), body.description());
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return ImmutableList.of(iterable, body);
    }
}
