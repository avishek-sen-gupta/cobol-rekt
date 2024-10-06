package org.smojol.common.transpiler.instruction;

import org.smojol.common.pseudocode.CodeSentinelType;
import org.smojol.common.transpiler.JumpTranspilerNode;
import org.smojol.common.transpiler.TranspilerInstruction;

public class JumpInstruction extends TranspilerInstruction {
    public JumpInstruction(JumpTranspilerNode node, CodeSentinelType sentinelType, String id) {
        super(node, sentinelType, id);
    }
}
