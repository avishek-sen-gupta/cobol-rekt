package org.smojol.common.vm.interpreter;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.ConditionVisitor;

public class ExpressionEvaluationResolver implements BooleanResolver {
    public static BooleanResolver EXPRESSION_EVALUATOR = new ExpressionEvaluationResolver();

    @Override
    public boolean resolve(FlowNode node, CobolParser.ConditionContext condition, FlowNodeService flowNodeService) {
        ConditionVisitor visitor = new ConditionVisitor(flowNodeService.getDataStructures());
        condition.accept(visitor);
        CobolExpression expression = visitor.getExpression();
        CobolExpression evaluatedResult = expression.evaluate(flowNodeService.getDataStructures());
        return evaluatedResult.evalAsBoolean(flowNodeService.getDataStructures());
    }
}
