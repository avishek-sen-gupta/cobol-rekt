package org.smojol.common.transpiler;

import org.smojol.common.pseudocode.CodeSentinelType;

public record TranspilerInstruction(TranspilerNode ref, CodeSentinelType sentinel, String id) {
    public static TranspilerInstruction NULL = new TranspilerInstruction(new NullTranspilerNode(), CodeSentinelType.BODY, "NO-ID");
}
