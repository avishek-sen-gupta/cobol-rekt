package org.smojol.cli;

import lombok.Getter;
import org.apache.commons.cli.*;
import org.smojol.analysis.LanguageDialect;
import org.smojol.interpreter.FlowchartGenerationStrategy;

import java.io.File;

@Getter
public class CliOptionsReader {
    private static final String HELP_LONG_OPTION = "help";
    private static final String SRC_LONG_OPTION = "src";
    private static final String SRC_DIR_LONG_OPTION = "srcDir";
    private static final String COPYBOOKS_DIR_LONG_OPTION = "copyBooksDir";
    private static final String DIALECT_JAR_PATH_LONG_OPTION = "dialectJarPath";
    private static final String REPORT_DIR_LONG_OPTION = "reportDir";
    private static final String DIALECT_LONG_OPTION = "dialect";
    private static final String GENERATION_STRATEGY_LONG_OPTION = "generation";
    private static final String EXCEPTION_TEMPLATE = "%s must be specified.";
    private static final String HELP_SMALL_OPTION = "h";
    private static final String SRC_SMALL_OPTION = "p";
    private static final String SRC_DIR_SMALL_OPTION = "s";
    private static final String COPYBOOKS_DIR_SMALL_OPTION = "c";
    private static final String DIALECT_JAR_PATH_SMALL_OPTION = "d";
    private static final String REPORT_DIR_SMALL_OPTION = "r";
    private static final String DIALECT_SMALL_OPTION = "x";
    private static final String GENERATION_STRATEGY_SMALL_OPTION = "g";
    private final Options options;
    private final HelpFormatter formatter;
    private String source;
    private String sourceDir;
    private File[] copyBookPaths;
    private String dialectJarPath;
    private String reportRootDir;
    private boolean isValid;
    private String dialectAsString;
    private LanguageDialect dialect;
    private String generationStrategyAsString;
    private FlowchartGenerationStrategy flowchartGenerationStrategy;

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
        if (!cmd.hasOption(DIALECT_LONG_OPTION) && !cmd.hasOption(DIALECT_SMALL_OPTION)) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", DIALECT_LONG_OPTION);
            return this.invalid();
        }
        if (!cmd.hasOption(SRC_LONG_OPTION) && !cmd.hasOption(SRC_SMALL_OPTION)) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", SRC_LONG_OPTION);
            return this.invalid();
        }
        if (!cmd.hasOption(SRC_DIR_LONG_OPTION) && !cmd.hasOption(SRC_DIR_SMALL_OPTION)) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", SRC_DIR_LONG_OPTION);
            return this.invalid();
        }
        if (!cmd.hasOption(COPYBOOKS_DIR_LONG_OPTION) && !cmd.hasOption(COPYBOOKS_DIR_SMALL_OPTION)) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", COPYBOOKS_DIR_LONG_OPTION);
            return this.invalid();
        }
        if (!cmd.hasOption(DIALECT_JAR_PATH_LONG_OPTION) && !cmd.hasOption(DIALECT_JAR_PATH_SMALL_OPTION)) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", DIALECT_JAR_PATH_LONG_OPTION);
            return this.invalid();
        }
        if (!cmd.hasOption(REPORT_DIR_LONG_OPTION) && !cmd.hasOption(REPORT_DIR_SMALL_OPTION)) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", REPORT_DIR_LONG_OPTION);
            return this.invalid();
        }
        if (!cmd.hasOption(GENERATION_STRATEGY_LONG_OPTION) && !cmd.hasOption(GENERATION_STRATEGY_SMALL_OPTION)) {
            System.out.printf((EXCEPTION_TEMPLATE) + "%n", GENERATION_STRATEGY_LONG_OPTION);
            return this.invalid();
        }

        source = cmd.getOptionValue(SRC_LONG_OPTION) != null ? cmd.getOptionValue(SRC_LONG_OPTION) : cmd.getOptionValue("p");
        sourceDir = cmd.getOptionValue(SRC_DIR_LONG_OPTION) != null ? cmd.getOptionValue(SRC_DIR_LONG_OPTION) : cmd.getOptionValue("s");
        copyBookPaths = new File[]{new File(cmd.getOptionValue(COPYBOOKS_DIR_LONG_OPTION) != null ? cmd.getOptionValue(COPYBOOKS_DIR_LONG_OPTION) : cmd.getOptionValue("c"))};
        dialectJarPath = cmd.getOptionValue(DIALECT_JAR_PATH_LONG_OPTION) != null ? cmd.getOptionValue(DIALECT_JAR_PATH_LONG_OPTION) : cmd.getOptionValue("d");
        reportRootDir = cmd.getOptionValue(REPORT_DIR_LONG_OPTION) != null ? cmd.getOptionValue(REPORT_DIR_LONG_OPTION) : cmd.getOptionValue("r");
        dialectAsString = cmd.getOptionValue(DIALECT_LONG_OPTION) != null ? cmd.getOptionValue(DIALECT_LONG_OPTION) : cmd.getOptionValue(DIALECT_SMALL_OPTION);
        generationStrategyAsString = cmd.getOptionValue(GENERATION_STRATEGY_LONG_OPTION) != null ? cmd.getOptionValue(GENERATION_STRATEGY_LONG_OPTION) : cmd.getOptionValue(GENERATION_STRATEGY_SMALL_OPTION);
        dialect = LanguageDialect.dialect(dialectAsString);
        flowchartGenerationStrategy = FlowchartGenerationStrategy.strategy(generationStrategyAsString);
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

    private CliOptionsReader valid() {
        isValid = true;
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
        options.addOption(DIALECT_SMALL_OPTION, DIALECT_LONG_OPTION, true, "The directory containing the final artifacts");
        options.addOption(GENERATION_STRATEGY_SMALL_OPTION, GENERATION_STRATEGY_LONG_OPTION, true, "The flowchart generation strategy (SECTION / PROGRAM)");
        return options;
    }
}
