package org.smojol.cli;

import lombok.Getter;
import org.apache.commons.cli.*;

import java.io.File;

@Getter
public class CliOptionsReader {
    private static final String SRC = "src";
    private static final String SRC_DIR = "srcDir";
    private static final String COPYBOOKS_DIR = "copyBooksDir";
    private static final String DIALECT_JAR_PATH = "dialectJarPath";
    private static final String REPORT_DIR = "reportDir";
    private static final String EXCEPTION_TEMPLATE = "%s must be specified.";
    private static final String HELP = "help";
    private String source;
    private String sourceDir;
    private File[] copyBookPaths;
    private String dialectJarPath;
    private String reportRootDir;

    public CliOptionsReader(String[] args) throws ParseException {
        Options options = new Options();
        options.addOption("h", HELP, false, "Help");
        options.addOption("p", SRC, true, "The name of the source file");
        options.addOption("s", SRC_DIR, true, "The directory containing source file");
        options.addOption("c", COPYBOOKS_DIR, true, "The directory containing copybooks");
        options.addOption("d", DIALECT_JAR_PATH, true, "The path to the dialect JAR");
        options.addOption("r", REPORT_DIR, true, "The directory containing the final artifacts");

        HelpFormatter formatter = new HelpFormatter();
        CommandLineParser parser = new DefaultParser();
        CommandLine cmd = parser.parse(options, args);

        if (cmd.hasOption(HELP) || cmd.hasOption("h")) {
            formatter.printHelp("smojol", options);
            return;
        }
        if (!cmd.hasOption(SRC) && !cmd.hasOption("p")) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", SRC);
            return;
        }
        if (!cmd.hasOption(SRC_DIR) && !cmd.hasOption("s")) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", SRC_DIR);
            return;
        }
        if (!cmd.hasOption(COPYBOOKS_DIR) && !cmd.hasOption("c")) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", COPYBOOKS_DIR);
            return;
        }
        if (!cmd.hasOption(DIALECT_JAR_PATH) && !cmd.hasOption("d")) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", DIALECT_JAR_PATH);
            return;
        }
        if (!cmd.hasOption(REPORT_DIR) && !cmd.hasOption("r")) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", REPORT_DIR);
            return;
        }

        source = cmd.getOptionValue(SRC) != null ? cmd.getOptionValue(SRC) : cmd.getOptionValue("p");
        sourceDir = cmd.getOptionValue(SRC_DIR) != null ? cmd.getOptionValue(SRC_DIR) : cmd.getOptionValue("s");
        copyBookPaths = new File[]{new File(cmd.getOptionValue(COPYBOOKS_DIR) != null ? cmd.getOptionValue(COPYBOOKS_DIR) : cmd.getOptionValue("c"))};
        dialectJarPath = cmd.getOptionValue(DIALECT_JAR_PATH) != null ? cmd.getOptionValue(DIALECT_JAR_PATH) : cmd.getOptionValue("d");
        reportRootDir = cmd.getOptionValue(REPORT_DIR) != null ? cmd.getOptionValue(REPORT_DIR) : cmd.getOptionValue("r");
    }
}
