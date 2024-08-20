package org.smojol.cli;

import com.google.common.collect.ImmutableList;
import org.smojol.toolkit.analysis.pipeline.LanguageDialect;

import java.io.File;

public class ValidateProgramMain {
    public static void main(String[] args) {
        new ValidateTaskRunner().processPrograms(
                ImmutableList.of("test-exp.cbl", "if-test.cbl"),
                "/Users/asgupta/code/smojol/smojol-test-code",
                LanguageDialect.IDMS,
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                "/Users/asgupta/code/smojol/out/validation.json");
    }
}
