package org.smojol.ast;

import org.eclipse.lsp.cobol.core.CobolParser;

public class CallTarget {
    private final CobolParser.CallStatementContext callStmt;

    public CallTarget(CobolParser.CallStatementContext callStmt) {
        this.callStmt = callStmt;
    }
}
