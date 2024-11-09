package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.logging.LoggingConfig;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.vm.structure.Format1DataStructureBuildStrategy;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;

public class TestTaskRunner {
    private final String programName;
    private final String sourceDir;

    public TestTaskRunner(String programName, String sourceDir) {
        this.programName = programName;
        this.sourceDir = sourceDir;
    }

    public AnalysisTaskResult runTask(CommandLineAnalysisTask task) throws IOException {
        return runTask2(task, new OccursIgnoringFormat1DataStructureBuilder());
    }

    public AnalysisTaskResult runTask2(CommandLineAnalysisTask task, Format1DataStructureBuildStrategy structureBuildStrategy) throws IOException {
        LoggingConfig.setupLogging();
        LocalFilesystemOperations resourceOperations = new LocalFilesystemOperations();
        UUIDProvider idProvider = new UUIDProvider();
        Map<String, List<AnalysisTaskResult>> results = new CodeTaskRunner(dir(sourceDir),
                dir("test-code/out"),
                ImmutableList.of(new File(dir(sourceDir))),
                dir("che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar"),
                LanguageDialect.COBOL, new FullProgram(FlowchartOutputFormat.MERMAID, idProvider), idProvider, structureBuildStrategy, new ProgramSearch(resourceOperations), resourceOperations)
                .runForPrograms(ImmutableList.of(task), ImmutableList.of(programName));
        return results.get(programName).get(1);
    }

    public static String dir(String path) {
        return Paths.get(System.getProperty("user.dir"), path).toString();
    }
}
