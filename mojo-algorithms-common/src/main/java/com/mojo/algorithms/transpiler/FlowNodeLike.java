package com.mojo.algorithms.transpiler;

import com.mojo.algorithms.CodeSentinelType;
import com.mojo.algorithms.id.Identifiable;

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
