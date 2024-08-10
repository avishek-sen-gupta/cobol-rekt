package org.smojol.ast;

import com.google.gson.annotations.Expose;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;

public abstract class CallTarget {
    @Expose private final String name;
    @Expose private final ReferenceType referenceType;

    public CallTarget(String callTargetString, ReferenceType referenceType) {
        this.name = callTargetString.trim().replace("\"", "").replace("'", "");
        this.referenceType = referenceType;
    }

    public static CallTarget target(CobolParser.CallStatementContext callStmt) {
        if (callStmt.constantName() != null) return new StaticCallTarget(callStmt.constantName().getText());
        return new DynamicCallTarget(callStmt.generalIdentifier());
    }

    static CallTarget target(IdmsParser.TransferStatementContext transfer) {
        if (transfer.idms_program_name() != null) return new StaticCallTarget(transfer.idms_program_name().getText());
        return new DynamicCallTarget(transfer.generalIdentifier().getFirst());
    }
}
