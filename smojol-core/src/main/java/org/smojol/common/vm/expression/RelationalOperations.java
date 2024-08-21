package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;

public enum RelationalOperations {
    EQUAL, NOT_EQUAL,
    GREATER_THAN, GREATER_THAN_EQUAL_TO,
    LESS_THAN, LESS_THAN_EQUAL_TO;

    public static ComparisonOperator create(CobolParser.RelationalOperatorContext operatorContext) {
        ComparisonOperator operation = operation(operatorContext);
        return operatorContext.NOT() != null ? operation.invert() : operation;
    }

    private static ComparisonOperator operation(CobolParser.RelationalOperatorContext operatorContext) {
        if (operatorContext.charBasedOperator() != null) return relation(operatorContext.charBasedOperator());
        else if (operatorContext.NOTEQUALCHAR() != null) return RelationalOperation.NOT_EQUAL;
        else if (operatorContext.GREATER() != null && operatorContext.EQUAL() != null) return RelationalOperation.GREATER_THAN_OR_EQUAL;
        else if (operatorContext.LESS() != null && operatorContext.EQUAL() != null) return RelationalOperation.LESS_THAN_OR_EQUAL;
        else if (operatorContext.LESSTHANOREQUAL() != null) return RelationalOperation.LESS_THAN_OR_EQUAL;
        else if (operatorContext.MORETHANOREQUAL() != null) return RelationalOperation.GREATER_THAN_OR_EQUAL;
        throw new UnsupportedOperationException("Not supported yet.");
    }

    private static ComparisonOperator relation(CobolParser.CharBasedOperatorContext context) {
        if (context.GREATER() != null || context.MORETHANCHAR() != null) return RelationalOperation.GREATER_THAN;
        else if (context.LESS() != null || context.LESSTHANCHAR() != null) return RelationalOperation.LESS_THAN;
        else if (context.EQUAL() != null || context.EQUALCHAR() != null) return RelationalOperation.EQUAL;
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
