package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

import java.util.Collection;

public class ListIterationTranspilerNode extends TranspilerNode {
    private final TranspilerNode iterable;
    private final TranspilerNode body;

    public ListIterationTranspilerNode(TranspilerNode iterable, TranspilerNode body) {
        super(ImmutableList.of(SemanticCategory.ITERATION, SemanticCategory.REFERENCE));
        this.iterable = iterable;
        this.body = body;
    }

    @Override
    public String description() {
        return String.format("iterate(%s) {\n%s\n}", iterable.description(), body.description());
    }

    @Override
    public Collection<TranspilerNode> astChildren() {
        return ImmutableList.of(body);
    }
}
