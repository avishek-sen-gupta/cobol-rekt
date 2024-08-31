package org.smojol.common.pseudocode;

import org.smojol.common.vm.expression.CobolExpression;

public class UnresolvedSymbolReferenceException extends RuntimeException {
    public UnresolvedSymbolReferenceException(CobolExpression expr) {
        super(expr.toString() + " is not resolved");
    }
}
