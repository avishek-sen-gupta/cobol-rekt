package org.smojol.common.transpiler.instruction;

import lombok.Getter;
import org.smojol.common.pseudocode.CodeSentinelType;
import org.smojol.common.transpiler.LocationNode;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.common.transpiler.TranspilerNode;

public class ConditionalJumpInstruction extends TranspilerInstruction {
    @Getter private final String name;
    private final LocationNode ifThenLabel;
    private final LocationNode ifElseLabel;

    public ConditionalJumpInstruction(TranspilerNode ref, LocationNode ifThenLabel, LocationNode ifElseLabel, CodeSentinelType sentinel, String id) {
        super(ref, sentinel, id);
        this.ifThenLabel = ifThenLabel;
        this.ifElseLabel = ifElseLabel;
        name = id;
    }
}
