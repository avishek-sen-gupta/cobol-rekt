package org.smojol.common.vm.interpreter;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;

public interface ConditionResolver {
    boolean resolveIf(FlowNode node, FlowNodeService nodeService);
    boolean resolveWhen(FlowNode whenPhrase, FlowNodeService nodeService);
    boolean resolveOn(FlowNode condition, FlowNodeService nodeService);
}
