package org.smojol.common.transpiler;

import java.util.List;

public record TranspilerModel(TranspilerNode tree, List<TranspilerInstruction> instructions, List<TranspilerEdge> instructionEdges) {}
