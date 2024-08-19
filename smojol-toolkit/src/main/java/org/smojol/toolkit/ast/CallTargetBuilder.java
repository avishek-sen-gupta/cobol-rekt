package org.smojol.toolkit.ast;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;
import org.smojol.common.ast.CallTarget;
import org.smojol.common.program.StaticCallTarget;

public class CallTargetBuilder {
    public static CallTarget target(CobolParser.CallStatementContext callStmt) {
        if (callStmt.constantName() != null) return new StaticCallTarget(callStmt.constantName().getText());
        return new DynamicCallTarget(callStmt.generalIdentifier());
    }

    static CallTarget target(IdmsParser.TransferStatementContext transfer) {
        if (transfer.idms_program_name() != null) return new StaticCallTarget(transfer.idms_program_name().getText());
        return new DynamicCallTarget(transfer.generalIdentifier().getFirst());
    }
}
