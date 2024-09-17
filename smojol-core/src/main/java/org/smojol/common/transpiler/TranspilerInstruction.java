package org.smojol.common.transpiler;

import org.smojol.common.pseudocode.CodeSentinelType;

public record TranspilerInstruction(TranspilerNode ref, CodeSentinelType body, String id) {
}
