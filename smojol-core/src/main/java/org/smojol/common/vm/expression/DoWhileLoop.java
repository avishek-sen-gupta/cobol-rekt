package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;

public class DoWhileLoop extends FlowLoop {
    private final CobolExpression terminatingCondition;

    public DoWhileLoop(CobolParser.ConditionContext terminatingCondition, CobolDataStructure dataStructures) {
        this.terminatingCondition = new CobolExpressionBuilder().condition(terminatingCondition, dataStructures);
    }
}
