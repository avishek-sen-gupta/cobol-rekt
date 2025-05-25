package org.smojol.toolkit.analysis.task.analysis;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.id.UUIDProvider;
import hu.webarticum.treeprinter.printer.listing.ListingTreePrinter;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.ast.CallTarget;
import org.smojol.common.program.CobolProgram;
import org.smojol.common.program.StaticCallTarget;
import org.smojol.common.flowchart.ConsoleColors;
import com.mojo.algorithms.visualisation.FlowchartOutputFormat;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.resource.ResourceOperations;
import com.mojo.algorithms.task.CommandLineAnalysisTask;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.AnalysisTaskResultError;
import com.mojo.algorithms.task.AnalysisTaskResultOK;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import static com.mojo.algorithms.task.CommandLineAnalysisTask.BUILD_BASE_ANALYSIS;

public class AnalyseProgramDependenciesTask {
    java.util.logging.Logger LOGGER = Logger.getLogger(AnalyseProgramDependenciesTask.class.getName());
    private final String sourceDir;
    private final List<File> copyBookPaths;
    private final String reportRootDir;
    private final String dialectJarPath;
    private final LanguageDialect dialect;
    private final ProgramSearch programSearch;
    private final ResourceOperations resourceOperations;

    public AnalyseProgramDependenciesTask(String sourceDir, List<File> copyBookPaths, String reportRootDir, String dialectJarPath, LanguageDialect dialect, ProgramSearch programSearch, ResourceOperations resourceOperations) {
        this.sourceDir = sourceDir;
        this.copyBookPaths = copyBookPaths;
        this.reportRootDir = reportRootDir;
        this.dialectJarPath = dialectJarPath;
        this.dialect = dialect;
        this.programSearch = programSearch;
        this.resourceOperations = resourceOperations;
    }

    private void recurse(CobolProgram program, List<File> copyBookPaths) throws IOException {
        Pair<File, String> searchResult = programSearch.run(program.getName(), sourceDir);
        if (searchResult == ProgramSearch.NO_PATH) return;
        File foundFile = searchResult.getLeft();
        String srcDir = searchResult.getRight();
        LOGGER.info("Found " + foundFile.getName() + " in " + srcDir);
        try {
            UUIDProvider idProvider = new UUIDProvider();
            Map<String, List<AnalysisTaskResult>> results = new CodeTaskRunner(srcDir,
                    reportRootDir, copyBookPaths, dialectJarPath,
                    dialect, new FullProgram(FlowchartOutputFormat.SVG, idProvider), idProvider, new OccursIgnoringFormat1DataStructureBuilder(), programSearch, resourceOperations)
                    .runForPrograms(ImmutableList.of(BUILD_BASE_ANALYSIS, CommandLineAnalysisTask.BUILD_PROGRAM_DEPENDENCIES), ImmutableList.of(foundFile.getName()));
            AnalysisTaskResult first = results.get(program.getName()).get(1);
            List<CallTarget> dependencies = switch (first) {
                case AnalysisTaskResultOK o -> o.getDetail();
                case AnalysisTaskResultError e -> throw new RuntimeException(e.getException());
            };
            program.addAll(dependencies.stream().map(CobolProgram::new).toList());

            for (CobolProgram childDependency : program.staticDependencies())
                recurse(childDependency, copyBookPaths);
        } catch (IOException | RuntimeException e) {
            LOGGER.severe(ConsoleColors.red("Error, terminating recursion down this path. Error: " + ExceptionUtils.getStackTrace(e)));
        }
    }

    public AnalysisTaskResult run(String programName) throws IOException {
        CobolProgram rootProgram = new CobolProgram(new StaticCallTarget(programName));
        recurse(rootProgram, copyBookPaths);
        new ListingTreePrinter().print(rootProgram);
        return AnalysisTaskResult.OK(CommandLineAnalysisTask.BUILD_PROGRAM_DEPENDENCIES, rootProgram);
    }
}
