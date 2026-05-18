package org.smojol.common.vm.expression;

import java.util.List;
import org.smojol.common.ast.FlowNode;

public record TestActionPair(CobolExpression test, List<FlowNode> actions) {}
