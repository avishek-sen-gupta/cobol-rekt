package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.resource.LocalFilesystemOperations;
import com.mojo.algorithms.graph.NamespaceQualifier;
import org.smojol.toolkit.intermediate.NodeSpecBuilder;
import org.smojol.toolkit.analysis.task.analysis.AnalyseProgramDependenciesTask;
import org.smojol.toolkit.analysis.task.analysis.InjectProgramDependenciesIntoNeo4JTask;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.AnalysisTaskResultError;
import com.mojo.algorithms.task.AnalysisTaskResultOK;
import org.smojol.common.program.CobolProgram;
import org.smojol.toolkit.analysis.pipeline.*;

import java.io.File;
import java.io.IOException;

public class DependencyBuildMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        AnalysisTaskResult result = new AnalyseProgramDependenciesTask("/Users/asgupta/code/smojol/smojol-test-code",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/out/report",
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS, new ProgramSearch(), new LocalFilesystemOperations())
                .run("if-test.cbl");
        CobolProgram root = switch (result) {
            case AnalysisTaskResultOK o -> o.getDetail();
            case AnalysisTaskResultError e -> throw new RuntimeException(e.getException());
        };

        new InjectProgramDependenciesIntoNeo4JTask(new NodeSpecBuilder(new NamespaceQualifier("DEP-GRAPH")),
                new GraphSDK(new Neo4JDriverBuilder().fromEnv())).run(root);
    }
}
