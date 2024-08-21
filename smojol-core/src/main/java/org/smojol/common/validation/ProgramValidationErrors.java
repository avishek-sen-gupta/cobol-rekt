package org.smojol.common.validation;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.common.error.SyntaxError;

import java.util.Arrays;
import java.util.List;

@Getter
public class ProgramValidationErrors {
    private final String programFileName;
    private final List<SyntaxError> syntaxErrors;
    private final List<String> referenceErrors;
    private final List<String> nonSyntaxErrors;

    public ProgramValidationErrors(String programFileName, List<SyntaxError> syntaxErrors, List<String> referenceErrors, ImmutableList<String> nonSyntaxErrors) {
        this.programFileName = programFileName;
        this.syntaxErrors = syntaxErrors;
        this.referenceErrors = referenceErrors;
        this.nonSyntaxErrors = nonSyntaxErrors;
    }

    private ProgramValidationErrors(String programFileName, SyntaxError[] syntaxErrors) {
        this(programFileName, Arrays.asList(syntaxErrors), ImmutableList.of(), ImmutableList.of());
    }

    private ProgramValidationErrors(String programFileName, String nonSyntaxError) {
        this(programFileName, ImmutableList.of(), ImmutableList.of(), ImmutableList.of(nonSyntaxError));
    }

    private ProgramValidationErrors(String programFileName) {
        this(programFileName, ImmutableList.of(), ImmutableList.of(), ImmutableList.of());
    }

    private ProgramValidationErrors(String programFilename, ParseTree[] unreferencedIdentifiers) {
        this(programFilename, ImmutableList.of(), Arrays.stream(unreferencedIdentifiers).map(ParseTree::getText).distinct().toList(), ImmutableList.of());
    }

    public static ProgramValidationErrors sourceNotFound(String programFilename) {
        return new ProgramValidationErrors(programFilename, "Not found");
    }

    public static ProgramValidationErrors noError(String programFileName) {
        return new ProgramValidationErrors(programFileName);
    }

    public static ProgramValidationErrors usageErrors(String programFilename, List<ParseTree> unreferencedIdentifiers) {
        return new ProgramValidationErrors(programFilename, unreferencedIdentifiers.toArray(new ParseTree[0]));
    }

    public static ProgramValidationErrors parseErrors(String programFilename, List<SyntaxError> errors) {
        return new ProgramValidationErrors(programFilename, errors.toArray(new SyntaxError[0]));
    }

    public Boolean IS_PARTIAL_SUCCESS() {
        return !hasSyntaxErrors() && !hasNonSyntaxErrors();
    }

    public Boolean IS_COMPLETE_SUCCESS() {
        return IS_PARTIAL_SUCCESS() && !hasReferenceErrors();
    }

    public Boolean hasNonSyntaxErrors() {
        return !nonSyntaxErrors.isEmpty();
    }

    public boolean hasReferenceErrors() {
        return !referenceErrors.isEmpty();
    }

    public boolean hasSyntaxErrors() {
        return !syntaxErrors.isEmpty();
    }
}
