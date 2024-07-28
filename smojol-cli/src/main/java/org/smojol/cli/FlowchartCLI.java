package org.smojol.cli;

import picocli.CommandLine;

/*
Use something like:
java -jar smojol-cli/target/smojol-cli.jar test-exp.cbl --srcDir /Users/asgupta/code/smojol/smojol-test-code --copyBooksDir /Users/asgupta/code/smojol/smojol-test-code --dialectJarPath /Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar --dialect COBOL --reportDir /Users/asgupta/code/smojol/out/report --generation PROGRAM
 */
public class FlowchartCLI {
    public static void main(String[] args) {
        int exitCode = new CommandLine(new MultiCommand()).execute(args);
        System.exit(exitCode);
    }
}
