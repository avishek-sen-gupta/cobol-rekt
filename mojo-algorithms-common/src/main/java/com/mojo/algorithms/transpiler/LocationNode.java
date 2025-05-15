package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.SemanticCategory;
import com.mojo.algorithms.domain.TranspilerNode;

public abstract class LocationNode extends TranspilerNode {
    public static LocationNode NULL = new LocationNode() {
        @Override
        public String name() {
            return "NULL";
        }

        @Override
        public String description() {
            return "[NULL]";
        }
    };
    public LocationNode() {
        super(ImmutableList.of(SemanticCategory.ADDRESS));
    }
    public abstract String name();
}
