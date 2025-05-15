package com.mojo.algorithms.domain;

import com.mojo.algorithms.id.InstructionLike;
import com.mojo.algorithms.transpiler.*;

import java.util.List;
import java.util.Objects;

public class TranspilerInstruction implements FlowNodeLike, InstructionLike {
    public static TranspilerInstruction NULL = new TranspilerInstruction(new NullTranspilerNode(), CodeSentinelType.BODY, "NO-ID");
    private final transient TranspilerNode ref;
    private final CodeSentinelType sentinel;
    private final String id;

    public TranspilerInstruction(TranspilerNode ref, CodeSentinelType sentinel, String id) {
        this.ref = ref;
        this.sentinel = sentinel;
        this.id = id;
    }

    @Override
    public String label() {
        return String.format("[%s] %s", sentinel.name(), ref.shortDescription());
    }

    @Override
    public String name() {
        return ref.getClass().getSimpleName();
    }

    @Override
    public String originalText() {
        return ref.description();
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.GENERIC_STATEMENT;
    }

    @Override
    public List<SemanticCategory> categories() {
        return ref.getCategories();
    }

    @Override
    public CodeSentinelType codeSentinelType() {
        return sentinel;
    }

    public boolean isJump() {
        return ref instanceof JumpTranspilerNode || ref instanceof ExitTranspilerNode;
    }

    public boolean isCondition() {
        return ref instanceof IfTranspilerNode;
    }

    @Override
    public String toString() {
        return String.format("[%s] (%s) %s", sentinel.name(), id, ref.description());
    }

    public TranspilerNode ref() {
        return ref;
    }

    @Override
    public CodeSentinelType sentinel() {
        return sentinel;
    }

    @Override
    public String id() {
        return id;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        var that = (TranspilerInstruction) obj;
        return Objects.equals(this.ref, that.ref)
               && Objects.equals(this.sentinel, that.sentinel)
               && Objects.equals(this.id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(ref, sentinel, id);
    }

    public String description() {
        return codeSentinelType().name() + ": " + ref.shortDescription();
    }
}
