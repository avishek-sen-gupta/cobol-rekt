package com.mojo.algorithms.transpiler;

import com.mojo.algorithms.domain.TranspilerNode;

public record TreeNodeLocation(TranspilerNode parentScope, TranspilerNode location) {
}
