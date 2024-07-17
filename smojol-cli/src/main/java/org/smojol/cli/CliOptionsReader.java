package org.smojol.cli;

import lombok.Getter;
import org.apache.commons.cli.*;

import java.io.File;

@Getter
public class CliOptionsReader {
    private static final String HELP_LONG_OPTION = "help";
    private static final String SRC_LONG_OPTION = "src";
    private static final String SRC_DIR_LONG_OPTION = "srcDir";
    private static final String COPYBOOKS_DIR_LONG_OPTION = "copyBooksDir";
    private static final String DIALECT_JAR_PATH_LONG_OPTION = "dialectJarPath";
    private static final String REPORT_DIR_LONG_OPTION = "reportDir";
    private static final String EXCEPTION_TEMPLATE = "%s must be specified.";
    private static final String HELP_SMALL_OPTION = "h";
    private static final String SRC_SMALL_OPTION = "p";
    private static final String SRC_DIR_SMALL_OPTION = "s";
    private static final String COPYBOOKS_DIR_SMALL_OPTION = "c";
    private static final String DIALECT_JAR_PATH_SMALL_OPTION = "d";
    private static final String REPORT_DIR_SMALL_OPTION = "r";
    private final Options options;
    private final HelpFormatter formatter;
    private String source;
    private String sourceDir;
    private File[] copyBookPaths;
    private String dialectJarPath;
    private String reportRootDir;
    private boolean isValid;

    public CliOptionsReader() {
        options = getOptions();
        formatter = new HelpFormatter();
    }

    public CliOptionsReader read(String[] args) throws ParseException {
        CommandLineParser parser = new DefaultParser();
        CommandLine cmd = parser.parse(options, args);

        if (cmd.hasOption(HELP_LONG_OPTION) || cmd.hasOption("h")) {
            return this.invalid();
        }
        if (!cmd.hasOption(SRC_LONG_OPTION) && !cmd.hasOption("p")) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", SRC_LONG_OPTION);
            return this.invalid();
        }
        if (!cmd.hasOption(SRC_DIR_LONG_OPTION) && !cmd.hasOption("s")) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", SRC_DIR_LONG_OPTION);
            return this.invalid();
        }
        if (!cmd.hasOption(COPYBOOKS_DIR_LONG_OPTION) && !cmd.hasOption("c")) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", COPYBOOKS_DIR_LONG_OPTION);
            return this.invalid();
        }
        if (!cmd.hasOption(DIALECT_JAR_PATH_LONG_OPTION) && !cmd.hasOption("d")) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", DIALECT_JAR_PATH_LONG_OPTION);
            return this.invalid();
        }
        if (!cmd.hasOption(REPORT_DIR_LONG_OPTION) && !cmd.hasOption("r")) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", REPORT_DIR_LONG_OPTION);
            return this.invalid();
        }

        source = cmd.getOptionValue(SRC_LONG_OPTION) != null ? cmd.getOptionValue(SRC_LONG_OPTION) : cmd.getOptionValue("p");
        sourceDir = cmd.getOptionValue(SRC_DIR_LONG_OPTION) != null ? cmd.getOptionValue(SRC_DIR_LONG_OPTION) : cmd.getOptionValue("s");
        copyBookPaths = new File[]{new File(cmd.getOptionValue(COPYBOOKS_DIR_LONG_OPTION) != null ? cmd.getOptionValue(COPYBOOKS_DIR_LONG_OPTION) : cmd.getOptionValue("c"))};
        dialectJarPath = cmd.getOptionValue(DIALECT_JAR_PATH_LONG_OPTION) != null ? cmd.getOptionValue(DIALECT_JAR_PATH_LONG_OPTION) : cmd.getOptionValue("d");
        reportRootDir = cmd.getOptionValue(REPORT_DIR_LONG_OPTION) != null ? cmd.getOptionValue(REPORT_DIR_LONG_OPTION) : cmd.getOptionValue("r");
        isValid = true;
        return this;
    }

    public void printUsage() {
        formatter.printHelp("java -jar smojol-cli/target/smojol-cli.jar", options);
    }

    private CliOptionsReader invalid() {
        isValid = false;
        return this;
    }

    private static Options getOptions() {
        Options options = new Options();
        options.addOption(HELP_SMALL_OPTION, HELP_LONG_OPTION, false, "Prints this help message");
        options.addOption(SRC_SMALL_OPTION, SRC_LONG_OPTION, true, "The name of the source file");
        options.addOption(SRC_DIR_SMALL_OPTION, SRC_DIR_LONG_OPTION, true, "The directory containing source file");
        options.addOption(COPYBOOKS_DIR_SMALL_OPTION, COPYBOOKS_DIR_LONG_OPTION, true, "The directory containing copybooks");
        options.addOption(DIALECT_JAR_PATH_SMALL_OPTION, DIALECT_JAR_PATH_LONG_OPTION, true, "The path to the dialect JAR");
        options.addOption(REPORT_DIR_SMALL_OPTION, REPORT_DIR_LONG_OPTION, true, "The directory containing the final artifacts");
        return options;
    }
}
