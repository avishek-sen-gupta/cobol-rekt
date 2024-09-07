package org.smojol.common.validation;

import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.smojol.common.flowchart.ConsoleColors;

import java.util.List;
import java.util.function.Function;
import java.util.logging.Logger;

public class ProgramValidationErrorReporter {
    private static final Logger LOGGER = Logger.getLogger(ProgramValidationErrorReporter.class.getName());
    public void reportPrograms(List<ProgramValidationErrors> validationErrors) {
        validationErrors.forEach(this::reportProgram);
    }

    private void reportProgram(ProgramValidationErrors ve) {
        if (ve.IS_COMPLETE_SUCCESS()) {
            LOGGER.info(ConsoleColors.green("No errors found for program: " + ve.getProgramFileName()));
            return;
        }

        reportSyntaxErrors(ve, SyntaxError::toString);
        LOGGER.info(String.format("Validation Summary: Program %s%n--------------------------------------------------------------%n", ve.getProgramFileName()));
        reportNonSyntaxErrors(ve);
        reportReferenceErrors(ve);
        reportSyntaxErrors(ve, e -> e.getErrorCode() + ": " + e.getSuggestion());
    }

    private void reportReferenceErrors(ProgramValidationErrors errors) {
        LOGGER.info(String.format("Reference Errors: Program %s%n--------------------------------------------------------------%n", errors.getProgramFileName()));
        if (!errors.hasReferenceErrors()) {
            LOGGER.info(ConsoleColors.green(String.format("No Syntax Errors for %s",
                    errors.getProgramFileName())));
            return;
        }
        String undefinedStructuresList = String.join(",", errors.getReferenceErrors());
        LOGGER.info(String.format("The following data structures referenced in the program were not defined: %s", undefinedStructuresList));
    }

    private void reportSyntaxErrors(ProgramValidationErrors singleProgramValidationErrors, Function<SyntaxError, String> format) {
        LOGGER.info(String.format("Validation Details: Program %s%n--------------------------------------------------------------%n", singleProgramValidationErrors.getProgramFileName()));
        if (!singleProgramValidationErrors.hasSyntaxErrors()) {
            LOGGER.info(ConsoleColors.green(String.format("No Syntax Errors for %s",
                    singleProgramValidationErrors.getProgramFileName())));
            return;
        }
        singleProgramValidationErrors.getSyntaxErrors().forEach(e -> LOGGER.info(String.format("%s", format.apply(e))));
    }

    private void reportNonSyntaxErrors(ProgramValidationErrors singleProgramValidationErrors) {
        if (!singleProgramValidationErrors.hasNonSyntaxErrors()) {
            LOGGER.info(ConsoleColors.green(String.format("No Non-Syntax Errors for %s",
                    singleProgramValidationErrors.getProgramFileName())));
            return;
        }
        for (String e : singleProgramValidationErrors.getNonSyntaxErrors()) {
            LOGGER.info(String.format("Non-Syntax Error for %s: %s", singleProgramValidationErrors.getProgramFileName(),
                    ConsoleColors.red(e)));
        }
    }
}
