package org.smojol.common.vm.expression;

import org.smojol.common.ast.FlowNode;

import java.util.List;

public record TestActionPair(CobolExpression test, List<FlowNode> actions) {
}
