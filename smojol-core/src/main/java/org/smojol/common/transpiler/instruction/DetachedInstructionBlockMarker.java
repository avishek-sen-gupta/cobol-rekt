package org.smojol.common.transpiler.instruction;

import lombok.Getter;
import org.smojol.common.pseudocode.CodeSentinelType;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.common.transpiler.TranspilerNode;

public class DetachedInstructionBlockMarker extends TranspilerInstruction {
    @Getter private final String name;

    public DetachedInstructionBlockMarker(TranspilerNode ref, CodeSentinelType sentinel, String id) {
        super(ref, sentinel, id);
        name = id;
    }
}
