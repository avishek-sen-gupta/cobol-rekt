package org.smojol.common.transpiler;

import org.smojol.common.id.Identifiable;
import org.smojol.common.pseudocode.CodeSentinelType;

public record TranspilerInstruction(TranspilerNode ref, CodeSentinelType sentinel, String id) implements Identifiable  {
    public static TranspilerInstruction NULL = new TranspilerInstruction(new NullTranspilerNode(), CodeSentinelType.BODY, "NO-ID");

    @Override
    public String label() {
        return String.format("[%s] %s", sentinel.name(), ref.description());
    }
}
