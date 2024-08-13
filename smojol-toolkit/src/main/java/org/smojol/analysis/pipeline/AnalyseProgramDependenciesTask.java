package org.smojol.analysis.pipeline;

import com.google.common.collect.ImmutableList;
import hu.webarticum.treeprinter.printer.listing.ListingTreePrinter;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.analysis.LanguageDialect;
import org.smojol.analysis.visualisation.CobolProgram;
import org.smojol.ast.ProgramDependencies;
import org.smojol.ast.StaticCallTarget;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.id.UUIDProvider;
import org.smojol.interpreter.FullProgram;
import org.smojol.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import static org.smojol.analysis.pipeline.CommandLineAnalysisTask.BUILD_PROGRAM_DEPENDENCIES;

public class AnalyseProgramDependenciesTask {
    private final String sourceDir;
    private final List<File> copyBookPaths;
    private final String reportRootDir;
    private final String dialectJarPath;

    public AnalyseProgramDependenciesTask(String sourceDir, List<File> copyBookPaths, String reportRootDir, String dialectJarPath) {
        this.sourceDir = sourceDir;
        this.copyBookPaths = copyBookPaths;
        this.reportRootDir = reportRootDir;
        this.dialectJarPath = dialectJarPath;
    }

    private void recurse(CobolProgram program, List<File> copyBookPaths) throws IOException {
        Pair<File, String> searchResult = new ProgramSearch().run(program.getName(), sourceDir);
        if (searchResult == ProgramSearch.NO_PATH) return;
        File foundFile = searchResult.getLeft();
        String srcDir = searchResult.getRight();
//        System.out.println("Searching for program: " + program.getName() + "...");
//        Collection<File> files = FileUtils.listFiles(new File(sourceDir), null, true);
//        List<File> matchingFiles = files.stream().filter(f -> program.getName().equals(f.getName())).toList();
//        if (matchingFiles.isEmpty()) {
//            System.out.println(ConsoleColors.red("No files matching " + program.getName() + " in " + sourceDir + ". Terminating recursion..."));
//            return;
//        }
//        File foundFile = matchingFiles.getFirst();
//        String srcDir = foundFile.getParent();
        System.out.println("Found " + foundFile.getName() + " in " + srcDir);
        try {
            Map<String, List<AnalysisTaskResult>> results = new CodeTaskRunner(srcDir,
                    reportRootDir, copyBookPaths, dialectJarPath,
                    LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.SVG), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder())
                    .generateForPrograms(ImmutableList.of(BUILD_PROGRAM_DEPENDENCIES), ImmutableList.of(program.getName()));
            AnalysisTaskResult first = results.get(program.getName()).getFirst();
            ProgramDependencies dependencies = switch (first) {
                case AnalysisTaskResultOK o -> (ProgramDependencies) o.getDetail();
                case AnalysisTaskResultError e -> throw new RuntimeException(e.getException());
            };
            program.addAll(dependencies.getDependencies().stream().map(CobolProgram::new).toList());

            for (CobolProgram childDependency : program.staticDependencies())
                recurse(childDependency, copyBookPaths);
        } catch (IOException | RuntimeException e) {
            System.out.println(ConsoleColors.red("Error, terminating recursion down this path. Error: " + e.getMessage()));
        }
    }

    public AnalysisTaskResult run(String programName) throws IOException {
        CobolProgram rootProgram = new CobolProgram(new StaticCallTarget(programName));
        recurse(rootProgram, copyBookPaths);
        new ListingTreePrinter().print(rootProgram);
        return AnalysisTaskResult.OK(BUILD_PROGRAM_DEPENDENCIES, rootProgram);
    }
}
