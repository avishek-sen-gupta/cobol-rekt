package org.smojol.analysis;

import lombok.Getter;
import org.eclipse.lsp.cobol.common.error.SyntaxError;

import java.util.List;
import java.util.Map;

@Getter
public class DiagnosticRuntimeError extends RuntimeException {
    private final Map<String, List<SyntaxError>> errorMap;

    public DiagnosticRuntimeError(Map<String, List<SyntaxError>> errorMap) {
        super("There were diagnostic errors while parsing");
        this.errorMap = errorMap;
    }

    @Override
    public String getMessage() {
        StringBuilder builder = new StringBuilder();
        DiagnosticRuntimeErrorFormatter errorFormatter = new DiagnosticRuntimeErrorFormatter();
        errorMap.forEach((key, errors) -> {
            builder.append("Program: " + key + "\n");
            builder.append(errorFormatter.formatted(errors));
        });
        return builder.toString();
    }
}
