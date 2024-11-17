package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

public class FunctionTranspilerNode extends TranspilerNode {
    private final String name;
    private final TranspilerCodeBlockNode body;

    public FunctionTranspilerNode(String name, TranspilerCodeBlockNode body) {
        super(ImmutableList.of(body), ImmutableList.of(SemanticCategory.FUNCTION));
        this.name = name;
        this.body = body;
    }
    @Override
    public String description() {
        return String.format("function %s()\n%s", name, body.description());
    }
}
