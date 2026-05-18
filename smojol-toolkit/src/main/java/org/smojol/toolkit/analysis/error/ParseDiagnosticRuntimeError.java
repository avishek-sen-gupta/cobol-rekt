package org.smojol.toolkit.analysis.error;

import java.util.List;
import lombok.Getter;
import org.eclipse.lsp.cobol.common.error.SyntaxError;

@Getter
public class ParseDiagnosticRuntimeError extends RuntimeException {
  private final List<SyntaxError> errors;

  public ParseDiagnosticRuntimeError(String message, List<SyntaxError> errors) {
    super(message);
    this.errors = errors;
  }
}
