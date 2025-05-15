package com.mojo.algorithms.transpiler;

import com.mojo.algorithms.domain.TranspilerNode;

public record TreeMatchDetail(boolean matched, boolean childMatcherCardinalitiesMatched, TranspilerNode actual) {
}
