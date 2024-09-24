package org.smojol.common.transpiler;

import org.smojol.common.ast.FlowNodeLike;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.ast.SemanticCategory;
import org.smojol.common.id.Identifiable;
import org.smojol.common.pseudocode.CodeSentinelType;

import java.util.List;

public record TranspilerInstruction(TranspilerNode ref, CodeSentinelType sentinel, String id) implements Identifiable, FlowNodeLike {
    public static TranspilerInstruction NULL = new TranspilerInstruction(new NullTranspilerNode(), CodeSentinelType.BODY, "NO-ID");

    @Override
    public String label() {
        return String.format("[%s] %s", sentinel.name(), ref.description());
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
}
