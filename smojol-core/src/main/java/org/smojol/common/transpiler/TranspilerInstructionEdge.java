package org.smojol.common.transpiler;

import java.util.Map;

public record TranspilerInstructionEdge(TranspilerInstruction from, TranspilerInstruction to, Map<String, String> metadata) {
}
