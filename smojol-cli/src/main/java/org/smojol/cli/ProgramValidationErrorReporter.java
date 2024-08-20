package org.smojol.cli;

import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.smojol.common.flowchart.ConsoleColors;

import java.util.List;
import java.util.function.Function;

public class ProgramValidationErrorReporter {
    public void reportPrograms(List<ProgramValidationErrors> validationErrors) {
        validationErrors.forEach(this::reportProgram);
    }

    private void reportProgram(ProgramValidationErrors ve) {
        if (ve.isSuccess()) {
            System.out.println(ConsoleColors.green("No errors found for program: " + ve.getProgramFileName()));
            return;
        }

        reportSyntaxErrors(ve, SyntaxError::toString);
        reportNonSyntaxErrors(ve);
        reportSyntaxErrors(ve, e -> e.getErrorCode() + ": " + e.getSuggestion());
    }

    private void reportSyntaxErrors(ProgramValidationErrors singleProgramValidationErrors, Function<SyntaxError, String> format) {
        String programFileName = singleProgramValidationErrors.getProgramFileName();
        System.out.printf("Program %s%n----------------------%n", programFileName);
        singleProgramValidationErrors.getSyntaxErrors().forEach(e -> {
            System.out.printf("%s%n", format.apply(e));
        });
    }

    private void reportNonSyntaxErrors(ProgramValidationErrors singleProgramValidationErrors) {
        if (!singleProgramValidationErrors.hasNonSyntaxErrors()) return;
        for (String e : singleProgramValidationErrors.getNonSyntaxErrors()) {
            System.out.printf("Non-Syntax Error for %s: %s%n", singleProgramValidationErrors.getProgramFileName(),
                    ConsoleColors.red(e));
        }
    }
}
