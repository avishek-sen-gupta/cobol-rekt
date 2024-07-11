package org.smojol.cli;

import com.google.common.collect.ImmutableList;
import org.apache.commons.cli.*;
import org.smojol.analysis.LanguageDialect;
import org.smojol.flowchart.FlowchartTasks;
import org.smojol.interpreter.FlowchartGenerationStrategy;

import java.io.File;
import java.io.IOException;
import java.util.List;

/*
Use something like:
java -jar smojol-cli/target/smojol-cli.jar --src test-exp.cbl --srcDir /Users/asgupta/code/smojol/smojol-test-code --copyBooksDir /Users/asgupta/code/smojol/smojol-test-code --dialectJarPath /Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar --reportDir /Users/asgupta/code/smojol/out/report
 */
public class FlowChartCLI {
    public static void main(String[] args) throws ParseException, IOException, InterruptedException {
        CliOptionsReader optionsReader = new CliOptionsReader(args);
        String source = optionsReader.getSource();
        String sourceDir = optionsReader.getSourceDir();
        File[] copyBookPaths = new File[]{new File(optionsReader.getDialectJarPath())};
        String dialectJarPath = optionsReader.getDialectJarPath();
        String reportRootDir = optionsReader.getReportRootDir();

        List<String> programNames = ImmutableList.of(source);
        new FlowchartTasks(sourceDir, reportRootDir, copyBookPaths, dialectJarPath).generateForPrograms(programNames, FlowchartGenerationStrategy.FULL_PROGRAM, LanguageDialect.COBOL);
    }
}
