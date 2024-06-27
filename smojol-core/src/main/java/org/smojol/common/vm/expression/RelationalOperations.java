package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;

public enum RelationalOperations {
    EQUAL, NOT_EQUAL,
    GREATER_THAN, GREATER_THAN_EQUAL_TO,
    LESS_THAN, LESS_THAN_EQUAL_TO;

    public static ComparisonOperator create(CobolParser.RelationalOperatorContext operatorContext) {
        ComparisonOperator operation = operation(operatorContext);
        return operatorContext.NOT() != null ? not(operation) : operation;
    }

    private static ComparisonOperator not(ComparisonOperator operation) {
        if (operation == RelationalOperation.EQUAL) return RelationalOperation.NOT_EQUAL;
        else if (operation == RelationalOperation.NOT_EQUAL) return RelationalOperation.EQUAL;
        else if (operation == RelationalOperation.LESS_THAN) return RelationalOperation.GREATER_THAN_OR_EQUAL;
        else if (operation == RelationalOperation.GREATER_THAN) return RelationalOperation.LESS_THAN_OR_EQUAL;
        else if (operation == RelationalOperation.LESS_THAN_OR_EQUAL) return RelationalOperation.GREATER_THAN;
        else if (operation == RelationalOperation.GREATER_THAN_OR_EQUAL) return RelationalOperation.LESS_THAN;
        throw new UnsupportedOperationException("Not supported yet.");
    }

    private static ComparisonOperator operation(CobolParser.RelationalOperatorContext operatorContext) {
        if (operatorContext.EQUAL() != null || operatorContext.EQUALCHAR() != null) return RelationalOperation.EQUAL;
        else if (operatorContext.NOTEQUALCHAR() != null) return RelationalOperation.NOT_EQUAL;
        else if (operatorContext.LESSTHANCHAR() != null) return RelationalOperation.LESS_THAN;
        else if (operatorContext.MORETHANCHAR() != null) return RelationalOperation.GREATER_THAN;
        else if (operatorContext.LESSTHANOREQUAL() != null) return RelationalOperation.LESS_THAN_OR_EQUAL;
        else if (operatorContext.MORETHANOREQUAL() != null) return RelationalOperation.GREATER_THAN_OR_EQUAL;
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
