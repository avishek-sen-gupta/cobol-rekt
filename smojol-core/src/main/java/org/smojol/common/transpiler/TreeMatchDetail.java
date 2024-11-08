package org.smojol.common.transpiler;

public record TreeMatchDetail(boolean matched, boolean childMatcherCardinalitiesMatched, TranspilerNode actual) {
}
