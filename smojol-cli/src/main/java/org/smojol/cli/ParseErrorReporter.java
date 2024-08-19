package org.smojol.cli;

import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.smojol.common.flowchart.ConsoleColors;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

public class ParseErrorReporter {
    private void output(Map<String, List<SyntaxError>> errorMap, Function<SyntaxError, String> format) {
        errorMap.forEach((programName, errors) -> {
            System.out.printf("Program %s%n----------------------%n", programName);
            errors.forEach(e -> System.out.printf("%s%n", format.apply(e)));
        });
    }

    public void report(Map<String, List<SyntaxError>> errorMap, List<String> programNames) {
        output(errorMap, SyntaxError::toString);
        output(errorMap, e -> e.getErrorCode() + ": " + e.getSuggestion());
        if (errorMap.isEmpty())
            System.out.println(ConsoleColors.green("No errors found for programs " + String.join(",", programNames)));
    }
}
