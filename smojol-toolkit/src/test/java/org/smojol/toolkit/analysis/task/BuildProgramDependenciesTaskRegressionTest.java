package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import org.junit.jupiter.api.Test;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.program.CobolProgram;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.AnalyseProgramDependenciesTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;

import java.io.File;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.smojol.toolkit.analysis.task.TestTaskRunner.dir;

public class BuildProgramDependenciesTaskRegressionTest {
    @Test
    void canBuildProgramDependencies() throws IOException {
        AnalysisTaskResult result = new AnalyseProgramDependenciesTask(dir("test-code/flow-ast"),
                ImmutableList.of(new File(dir("test-code/flow-ast"))),
                dir("test-code/flow-ast/out"),
                dir("che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar"),
                LanguageDialect.COBOL, new ProgramSearch(), new LocalFilesystemOperations())
                .run("no-branches.cbl");
        assertTrue(result.isSuccess());
        CobolProgram root = ((AnalysisTaskResultOK) result).getDetail();
    }
}
