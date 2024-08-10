package org.smojol.analysis.pipeline;

import com.google.common.collect.ImmutableList;
import hu.webarticum.treeprinter.printer.listing.ListingTreePrinter;
import org.smojol.analysis.LanguageDialect;
import org.smojol.analysis.visualisation.CobolProgram;
import org.smojol.ast.ProgramDependencies;
import org.smojol.ast.StaticCallTarget;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.id.UUIDProvider;
import org.smojol.interpreter.FullProgram;
import org.smojol.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import static org.smojol.analysis.pipeline.CommandLineAnalysisTask.BUILD_PROGRAM_DEPENDENCIES;

public class AnalyseDependenciesTask {
    private final String sourceDir;
    private final List<File> copyBookPaths;
    private final String reportRootDir;
    private final String dialectJarPath;

    public AnalyseDependenciesTask(String sourceDir, List<File> copyBookPaths, String reportRootDir, String dialectJarPath) {
        this.sourceDir = sourceDir;
        this.copyBookPaths = copyBookPaths;
        this.reportRootDir = reportRootDir;
        this.dialectJarPath = dialectJarPath;
    }

    private void recurse(CobolProgram program, List<File> copyBookPaths) throws IOException {
        Map<String, List<AnalysisTaskResult>> results = new CodeTaskRunner(sourceDir,
                reportRootDir, copyBookPaths, dialectJarPath,
                LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.SVG), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder())
                .generateForPrograms(ImmutableList.of(BUILD_PROGRAM_DEPENDENCIES), ImmutableList.of(program.getName()));
        AnalysisTaskResult first = results.get(program.getName()).getFirst();
        ProgramDependencies dependencies = (ProgramDependencies) first.getDetail();
        program.addAll(dependencies.getDependencies().stream().map(CobolProgram::new).toList());

        for (CobolProgram childDependency : program.staticDependencies())
            recurse(childDependency, copyBookPaths);
    }

    public CobolProgram run(String programName) throws IOException {
        CobolProgram rootProgram = new CobolProgram(new StaticCallTarget(programName));
        recurse(rootProgram, copyBookPaths);
        new ListingTreePrinter().print(rootProgram);
        return rootProgram;
    }
}
