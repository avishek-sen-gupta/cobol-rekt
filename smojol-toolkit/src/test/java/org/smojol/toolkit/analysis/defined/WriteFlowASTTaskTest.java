package org.smojol.toolkit.analysis.defined;

import com.google.common.collect.ImmutableList;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.resource.JarResourceOperations;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

class WriteFlowASTTaskTest {
    @Test
    @Disabled
    void canCreateFlowAST() throws IOException {
        LocalFilesystemOperations resourceOperations = new LocalFilesystemOperations();
//        ResourceOperations resourceOperations = new JarResourceOperations();
        Map<String, List<AnalysisTaskResult>> result = new CodeTaskRunner("smojol-test-code/flow-ast",
                "no/report",
                ImmutableList.of(new File("smojol-test-code/flow-ast")),
                "che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.COBOL, new FullProgram(FlowchartOutputFormat.MERMAID), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(resourceOperations), resourceOperations)
                .runForPrograms(ImmutableList.of(CommandLineAnalysisTask.WRITE_FLOW_AST), ImmutableList.of("no-branches.cbl"));

    }
}
