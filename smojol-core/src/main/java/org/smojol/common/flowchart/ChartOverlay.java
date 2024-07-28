package org.smojol.common.flowchart;

import org.smojol.common.ast.FlowNode;

public interface ChartOverlay {
    FlowNode block(FlowNode node);
}
