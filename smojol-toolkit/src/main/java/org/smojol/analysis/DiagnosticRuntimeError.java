package org.smojol.analysis;

import lombok.Getter;
import org.eclipse.lsp.cobol.common.error.SyntaxError;

import java.util.List;

@Getter
public class DiagnosticRuntimeError extends RuntimeException {
    private final List<SyntaxError> errors;

    public DiagnosticRuntimeError(String message, List<SyntaxError> errors) {
        super(message);
        this.errors = errors;
    }
}
