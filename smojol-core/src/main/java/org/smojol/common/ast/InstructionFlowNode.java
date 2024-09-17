package org.smojol.common.ast;

import org.smojol.common.pseudocode.CodeSentinelType;
import org.smojol.common.pseudocode.PseudocodeInstruction;

import java.util.List;

public class InstructionFlowNode implements FlowNodeLike {
    private final PseudocodeInstruction instruction;

    public InstructionFlowNode(PseudocodeInstruction instruction) {
        this.instruction = instruction;
    }

    @Override
    public String id() {
        return instruction.id();
    }

    @Override
    public String label() {
        return instruction.label();
    }

    @Override
    public String name() {
        return instruction.name();
    }

    @Override
    public String originalText() {
        return instruction.originalText();
    }

    @Override
    public FlowNodeType type() {
        return instruction.type();
    }

    @Override
    public List<SemanticCategory> categories() {
        return instruction.categories();
    }

    @Override
    public CodeSentinelType codeSentinelType() {
        return instruction.codeSentinelType();
    }
}
