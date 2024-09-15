package org.smojol.toolkit.analysis.validation;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.common.validation.ProgramValidationErrorReporter;
import org.smojol.common.validation.ProgramValidationErrors;
import org.smojol.toolkit.analysis.pipeline.*;
import org.smojol.toolkit.analysis.error.ParseDiagnosticRuntimeError;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
import org.smojol.common.dependency.ComponentsBuilder;
import org.smojol.toolkit.ast.FlowchartBuilderImpl;
import org.smojol.common.ast.CobolTreeVisualiser;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.vm.strategy.UnresolvedReferenceDoNothingStrategy;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

import java.io.*;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.Function;
import java.util.logging.Logger;

public class ValidateTaskRunner {
    java.util.logging.Logger LOGGER = Logger.getLogger(ValidateTaskRunner.class.getName());
    private final ProgramSearch programSearch;
    private final ResourceOperations resourceOperations;

    public ValidateTaskRunner() {
        this(new ProgramSearch());
    }

    public ValidateTaskRunner(ProgramSearch programSearch) {
        this(programSearch, new LocalFilesystemOperations());
    }

    public ValidateTaskRunner(ProgramSearch programSearch, ResourceOperations resourceOperations) {
        this.programSearch = programSearch;
        this.resourceOperations = resourceOperations;
    }
    public ValidateTaskRunner(ResourceOperations resourceOperations) {
        this(new ProgramSearch(resourceOperations), resourceOperations);
    }

    public boolean processPrograms(List<String> programNames, String sourceDir, LanguageDialect dialect, List<File> copyBookPaths, String dialectJarPath, String outputPath) {
        return processPrograms(programNames, sourceDir, dialect, copyBookPaths, dialectJarPath, outputPath, ProgramValidationErrors::IS_PARTIAL_SUCCESS, DataStructureValidation.BUILD);
    }

    public boolean processPrograms(List<String> programNames, String sourceDir, LanguageDialect dialect, List<File> copyBookPaths, String dialectJarPath, String outputPath, Function<ProgramValidationErrors, Boolean> successCriterion, DataStructureValidation dataStructureValidation) {
        String absoluteDialectJarPath = Paths.get(dialectJarPath).toAbsolutePath().normalize().toString();
        List<ProgramValidationErrors> validationErrors = programNames.stream().map(p -> run(copyBookPaths, absoluteDialectJarPath, p, sourceDir, dialect, dataStructureValidation)).toList();
        List<Boolean> allResults = validationErrors.stream().map(successCriterion).toList();
        new ProgramValidationErrorReporter().reportPrograms(validationErrors);
        if (outputPath != null) writeToFile(validationErrors, outputPath);
        return allResults.stream().reduce(true, (all, b) -> all && b);
    }

    private void writeToFile(List<ProgramValidationErrors> validationErrors, String outputPath) {
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        String json = gson.toJson(validationErrors);
        LOGGER.info("Writing to: " + outputPath);
        try (PrintWriter out = new PrintWriter(outputPath)) {
            out.println(json);
            LOGGER.info("Wrote to: " + outputPath);
        } catch (FileNotFoundException e) {
            LOGGER.severe(ConsoleColors.red("Error writing to file: " + e.getMessage()));
        }
    }

    public ProgramValidationErrors run(List<File> copyBookPaths, String absoluteDialectJarPath, String programFilename, String sourceDir, LanguageDialect dialect, DataStructureValidation dataStructureValidation) {
        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiser(),
                FlowchartBuilderImpl::build, new EntityNavigatorBuilder(), new UnresolvedReferenceDoNothingStrategy(),
                new OccursIgnoringFormat1DataStructureBuilder(), new UUIDProvider(), resourceOperations);

        Pair<File, String> programPath = programSearch.run(programFilename, sourceDir);
        if (programPath == ProgramSearch.NO_PATH) {
            return ProgramValidationErrors.sourceNotFound(programFilename);
        }

        SourceConfig sourceConfig = new SourceConfig(programFilename, programPath.getRight(), copyBookPaths, absoluteDialectJarPath);
        ParsePipeline pipeline = new ParsePipeline(sourceConfig, ops, dialect);
        try {
            CobolEntityNavigator navigator = pipeline.parse(dataStructureValidation);
            return dataStructureValidation.validate(navigator, pipeline, programFilename);
        } catch (ParseDiagnosticRuntimeError e) {
            return ProgramValidationErrors.parseErrors(programFilename, e.getErrors());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
