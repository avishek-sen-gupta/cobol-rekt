package org.smojol.common.validation;

import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.smojol.common.flowchart.ConsoleColors;

import java.util.List;
import java.util.function.Function;

public class ProgramValidationErrorReporter {
    public void reportPrograms(List<ProgramValidationErrors> validationErrors) {
        validationErrors.forEach(this::reportProgram);
    }

    private void reportProgram(ProgramValidationErrors ve) {
        if (ve.IS_COMPLETE_SUCCESS()) {
            System.out.println(ConsoleColors.green("No errors found for program: " + ve.getProgramFileName()));
            return;
        }

        reportSyntaxErrors(ve, SyntaxError::toString);
        System.out.printf("Validation Summary: Program %s%n--------------------------------------------------------------%n", ve.getProgramFileName());
        reportNonSyntaxErrors(ve);
        reportReferenceErrors(ve);
        reportSyntaxErrors(ve, e -> e.getErrorCode() + ": " + e.getSuggestion());
    }

    private void reportReferenceErrors(ProgramValidationErrors errors) {
        System.out.printf("Reference Errors: Program %s%n--------------------------------------------------------------%n", errors.getProgramFileName());
        if (!errors.hasReferenceErrors()) {
            System.out.println(ConsoleColors.green(String.format("No Syntax Errors for %s%n",
                    errors.getProgramFileName())));
            return;
        }
        String undefinedStructuresList = String.join(",", errors.getReferenceErrors());
        System.out.printf("The following data structures referenced in the program were not defined: %s%n", undefinedStructuresList);
    }

    private void reportSyntaxErrors(ProgramValidationErrors singleProgramValidationErrors, Function<SyntaxError, String> format) {
        System.out.printf("Validation Details: Program %s%n--------------------------------------------------------------%n", singleProgramValidationErrors.getProgramFileName());
        if (!singleProgramValidationErrors.hasSyntaxErrors()) {
            System.out.println(ConsoleColors.green(String.format("No Syntax Errors for %s%n",
                    singleProgramValidationErrors.getProgramFileName())));
            return;
        }
        singleProgramValidationErrors.getSyntaxErrors().forEach(e -> System.out.printf("%s%n", format.apply(e)));
    }

    private void reportNonSyntaxErrors(ProgramValidationErrors singleProgramValidationErrors) {
        if (!singleProgramValidationErrors.hasNonSyntaxErrors()) {
            System.out.println(ConsoleColors.green(String.format("No Non-Syntax Errors for %s%n",
                    singleProgramValidationErrors.getProgramFileName())));
            return;
        }
        for (String e : singleProgramValidationErrors.getNonSyntaxErrors()) {
            System.out.printf("Non-Syntax Error for %s: %s%n", singleProgramValidationErrors.getProgramFileName(),
                    ConsoleColors.red(e));
        }
    }
}
