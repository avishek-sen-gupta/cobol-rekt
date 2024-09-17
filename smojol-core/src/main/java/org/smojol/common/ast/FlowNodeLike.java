package org.smojol.common.ast;

import org.smojol.common.id.Identifiable;
import org.smojol.common.pseudocode.CodeSentinelType;

import java.util.List;

public interface FlowNodeLike extends Identifiable {
    String id();
    String label();
    String name();
    String originalText();
    FlowNodeType type();
    List<SemanticCategory> categories();
    CodeSentinelType codeSentinelType();
}
