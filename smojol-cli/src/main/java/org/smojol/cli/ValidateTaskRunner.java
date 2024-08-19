package org.smojol.cli;

import com.google.common.collect.ImmutableList;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.smojol.toolkit.analysis.pipeline.LanguageDialect;
import org.smojol.toolkit.analysis.error.ParseDiagnosticRuntimeError;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
import org.smojol.common.dependency.ComponentsBuilder;
import org.smojol.toolkit.ast.FlowchartBuilderImpl;
import org.smojol.common.ast.CobolTreeVisualiser;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.vm.strategy.UnresolvedReferenceDoNothingStrategy;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ValidateTaskRunner {
    public boolean processPrograms(List<String> programNames, String sourceDir, LanguageDialect dialect, List<File> copyBookPaths, String dialectJarPath) {
        String absoluteDialectJarPath = Paths.get(dialectJarPath).toAbsolutePath().normalize().toString();
        List<Boolean> allResults = programNames.stream().map(p -> run(copyBookPaths, absoluteDialectJarPath, p, sourceDir, dialect)).toList();
        return allResults.stream().reduce(true, (all, b) -> all && b);
    }

    public boolean run(List<File> copyBookPaths, String absoluteDialectJarPath, String programFilename, String sourceDir, LanguageDialect dialect) {
        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiser(),
                FlowchartBuilderImpl::build, new EntityNavigatorBuilder(), new UnresolvedReferenceDoNothingStrategy(),
                new OccursIgnoringFormat1DataStructureBuilder(), new UUIDProvider());

        Pair<File, String> programPath = new ProgramSearch().run(programFilename, sourceDir);
        if (programPath == ProgramSearch.NO_PATH) {
            System.out.printf("No program found for '%s' anywhere in path %s \n", programFilename, sourceDir);
            return false;
        }

        SourceConfig sourceConfig = new SourceConfig(programFilename, programPath.getRight(), copyBookPaths, absoluteDialectJarPath);
        ParsePipeline pipeline = new ParsePipeline(sourceConfig, ops, dialect);
        Map<String, List<SyntaxError>> errorMap = new HashMap<>();
        try {
            pipeline.parse();
        } catch (ParseDiagnosticRuntimeError e) {
            errorMap.put(programFilename, e.getErrors());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        new ParseErrorReporter().report(errorMap, ImmutableList.of(programFilename));
        return errorMap.isEmpty();
    }
}
