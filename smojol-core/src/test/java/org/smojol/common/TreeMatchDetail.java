package org.smojol.common;

import org.smojol.common.transpiler.TranspilerNode;

public record TreeMatchDetail(boolean matched, boolean childMatcherCardinalitiesMatched, TranspilerNode actual) {
}
