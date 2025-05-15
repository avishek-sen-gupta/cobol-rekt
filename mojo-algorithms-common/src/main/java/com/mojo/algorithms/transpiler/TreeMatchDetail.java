package com.mojo.algorithms.transpiler;

public record TreeMatchDetail(boolean matched, boolean childMatcherCardinalitiesMatched, TranspilerNode actual) {
}
