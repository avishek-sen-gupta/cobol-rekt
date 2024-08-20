package org.smojol.cli;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.flowchart.ConsoleColors;
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

public class ValidateTaskRunner {
    public boolean processPrograms(List<String> programNames, String sourceDir, LanguageDialect dialect, List<File> copyBookPaths, String dialectJarPath, String outputPath) {
        String absoluteDialectJarPath = Paths.get(dialectJarPath).toAbsolutePath().normalize().toString();
        List<ProgramValidationErrors> validationErrors = programNames.stream().map(p -> run(copyBookPaths, absoluteDialectJarPath, p, sourceDir, dialect)).toList();
        List<Boolean> allResults = validationErrors.stream().map(ProgramValidationErrors::isSuccess).toList();
        new ProgramValidationErrorReporter().reportPrograms(validationErrors);
        if (outputPath != null) writeToFile(validationErrors, outputPath);
        return allResults.stream().reduce(true, (all, b) -> all && b);
    }

    private void writeToFile(List<ProgramValidationErrors> validationErrors, String outputPath) {
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        String json = gson.toJson(validationErrors);
        System.out.println("Writing to: " + outputPath);
        try (PrintWriter out = new PrintWriter(outputPath)) {
            out.println(json);
            System.out.println("Wrote to: " + outputPath);
        } catch (FileNotFoundException e) {
            System.out.println(ConsoleColors.red("Error writing to file: " + e.getMessage()));
        }
    }

    public ProgramValidationErrors run(List<File> copyBookPaths, String absoluteDialectJarPath, String programFilename, String sourceDir, LanguageDialect dialect) {
        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiser(),
                FlowchartBuilderImpl::build, new EntityNavigatorBuilder(), new UnresolvedReferenceDoNothingStrategy(),
                new OccursIgnoringFormat1DataStructureBuilder(), new UUIDProvider());

        Pair<File, String> programPath = new ProgramSearch().run(programFilename, sourceDir);
        if (programPath == ProgramSearch.NO_PATH) {
//            System.out.printf("No program found for '%s' anywhere in path %s \n", programFilename, sourceDir);
            return ProgramValidationErrors.sourceNotFound(programFilename);
        }

        SourceConfig sourceConfig = new SourceConfig(programFilename, programPath.getRight(), copyBookPaths, absoluteDialectJarPath);
        ParsePipeline pipeline = new ParsePipeline(sourceConfig, ops, dialect);
        try {
            pipeline.parse();
            return ProgramValidationErrors.noError(programFilename);
        } catch (ParseDiagnosticRuntimeError e) {
            return new ProgramValidationErrors(programFilename, e.getErrors());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
