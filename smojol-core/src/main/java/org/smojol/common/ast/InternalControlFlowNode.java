package org.smojol.common.ast;

import java.util.List;

public interface InternalControlFlowNode {
    List<FlowNode> callTargets();
}
