package org.smojol.cli;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.eclipse.lsp.cobol.common.error.SyntaxError;

import java.util.List;

@Getter
public class ProgramValidationErrors {
    private final String programFileName;
    private final List<SyntaxError> syntaxErrors;
    private final String[] nonSyntaxErrors;

    public ProgramValidationErrors(String programFileName, List<SyntaxError> syntaxErrors) {
        this.programFileName = programFileName;
        this.syntaxErrors = syntaxErrors;
        this.nonSyntaxErrors = new String[0];
    }

    public ProgramValidationErrors(String programFileName, String error) {
        this.programFileName = programFileName;
        this.syntaxErrors = ImmutableList.of();
        this.nonSyntaxErrors = new String[]{error};
    }

    public ProgramValidationErrors(String programFileName) {
        this.programFileName = programFileName;
        this.syntaxErrors = ImmutableList.of();
        this.nonSyntaxErrors = new String[0];
    }

    public static ProgramValidationErrors sourceNotFound(String programFilename) {
        return new ProgramValidationErrors(programFilename, "Not found");
    }

    public static ProgramValidationErrors noError(String programFileName) {
        return new ProgramValidationErrors(programFileName);
    }

    public boolean isSuccess() {
        return syntaxErrors.isEmpty() && nonSyntaxErrors.length == 0;
    }

    public boolean hasNonSyntaxErrors() {
        return nonSyntaxErrors.length > 0;
    }
}
