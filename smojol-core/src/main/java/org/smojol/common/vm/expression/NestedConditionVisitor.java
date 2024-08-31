package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;

public class NestedConditionVisitor extends AntlrCobolExpressionVisitor {
    private final CobolDataStructure dataRoot;

    public NestedConditionVisitor(CobolDataStructure dataRoot) {
        this.dataRoot = dataRoot;
    }

    @Override
    public CobolExpression visitNestedCondition(CobolParser.NestedConditionContext ctx) {
        ConditionVisitor conditionVisitor = new ConditionVisitor(dataRoot);
        ctx.condition().accept(conditionVisitor);
        expression = conditionVisitor.getExpression();
        return expression;
    }
}
