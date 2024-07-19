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
java -jar smojol-cli/target/smojol-cli.jar --src test-exp.cbl --srcDir /Users/asgupta/code/smojol/smojol-test-code --copyBooksDir /Users/asgupta/code/smojol/smojol-test-code --dialectJarPath /Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar --dialect COBOL --reportDir /Users/asgupta/code/smojol/out/report --generation PROGRAM
 */
public class FlowchartCLI {
    public static void main(String[] args) throws ParseException, IOException, InterruptedException {
        CliOptionsReader optionsReader = new CliOptionsReader().read(args);
        if (!optionsReader.isValid()) {
            optionsReader.printUsage();
            return;
        }
        String source = optionsReader.getSource();
        String sourceDir = optionsReader.getSourceDir();
        File[] copyBookPaths = optionsReader.getCopyBookPaths();
        String dialectJarPath = optionsReader.getDialectJarPath();
        String reportRootDir = optionsReader.getReportRootDir();
        LanguageDialect dialect = optionsReader.getDialect();
        FlowchartGenerationStrategy flowchartGenerationStrategy = optionsReader.getFlowchartGenerationStrategy();

        List<String> programNames = ImmutableList.of(source);
        new FlowchartTasks(sourceDir, reportRootDir, copyBookPaths, dialectJarPath).generateForPrograms(programNames, flowchartGenerationStrategy, dialect);
    }
}
