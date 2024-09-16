package org.smojol.toolkit.analysis.defined;

import com.google.common.collect.ImmutableList;
import org.junit.jupiter.api.Test;
import org.smojol.common.ast.SerialisableASTFlowNode;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.logging.LoggingConfig;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertTrue;

class WriteFlowASTTaskTest {
    @Test
    void canCreateFlowAST() throws IOException {
        LoggingConfig.setupLogging();
        LocalFilesystemOperations resourceOperations = new LocalFilesystemOperations();
        Map<String, List<AnalysisTaskResult>> results = new CodeTaskRunner(dir("test-code/flow-ast"),
                dir("test-code/out"),
                ImmutableList.of(new File(dir(("test-code/flow-ast")))),
                dir("che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar"),
                LanguageDialect.COBOL, new FullProgram(FlowchartOutputFormat.MERMAID), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(resourceOperations), resourceOperations)
                .runForPrograms(ImmutableList.of(CommandLineAnalysisTask.WRITE_FLOW_AST), ImmutableList.of("no-branches.cbl"));
        AnalysisTaskResult taskResult = results.get("no-branches.cbl").getFirst();
        assertTrue(taskResult.isSuccess());
        SerialisableASTFlowNode root = ((AnalysisTaskResultOK) taskResult).getDetail();
    }

    private static String dir(String path) {
        return Paths.get(System.getProperty("user.dir"), path).toString();
    }
}
