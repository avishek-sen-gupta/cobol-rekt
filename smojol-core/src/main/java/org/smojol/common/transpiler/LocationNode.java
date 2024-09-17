package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

public abstract class LocationNode extends TranspilerNode {
    public LocationNode() {
        super(ImmutableList.of(SemanticCategory.ADDRESS));
    }
}
