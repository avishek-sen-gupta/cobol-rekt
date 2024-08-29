package org.smojol.common.ast;

import java.util.List;

public interface FlowNodeLike {
    String id();
    String label();
    String name();
    String originalText();
    FlowNodeType type();
    List<FlowNodeCategory> categories();
}
