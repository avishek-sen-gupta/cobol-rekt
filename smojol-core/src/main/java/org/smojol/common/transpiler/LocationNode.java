package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

public abstract class LocationNode extends TranspilerNode {
    public static LocationNode NULL = new LocationNode() {
        @Override
        public String description() {
            return "[NULL]";
        }
    };
    public LocationNode() {
        super(ImmutableList.of(SemanticCategory.ADDRESS));
    }
}
