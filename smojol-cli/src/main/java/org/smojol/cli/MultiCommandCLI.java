package org.smojol.cli;

import picocli.CommandLine;

/*
Use something like:
java -jar smojol-cli/target/smojol-cli.jar --commands="INJECT_INTO_NEO4J DRAW_FLOWCHART" --srcDir /Users/asgupta/code/smojol/smojol-test-code --copyBooksDir /Users/asgupta/code/smojol/smojol-test-code --dialectJarPath ./che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar --reportDir out/report --generation=PROGRAM */
public class MultiCommandCLI {
    public static void main(String[] args) {
        int exitCode = new CommandLine(new MultiCommand()).execute(args);
        System.exit(exitCode);
    }
}
