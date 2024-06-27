package org.smojol.common.vm.interpreter;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;

public interface BooleanResolver {
    boolean resolve(FlowNode node, CobolParser.ConditionContext condition, FlowNodeService nodeService);
}
