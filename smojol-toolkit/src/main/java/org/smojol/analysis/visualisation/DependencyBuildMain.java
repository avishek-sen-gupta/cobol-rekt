package org.smojol.analysis.visualisation;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import org.smojol.analysis.graph.NamespaceQualifier;
import org.smojol.analysis.graph.NodeSpecBuilder;
import org.smojol.analysis.pipeline.*;

import java.io.File;
import java.io.IOException;

public class DependencyBuildMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        AnalysisTaskResult result = new AnalyseProgramDependenciesTask("/Users/asgupta/code/smojol/smojol-test-code",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/out/report",
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar")
                .run("if-test.cbl");
        CobolProgram root = switch (result) {
            case AnalysisTaskResultOK o -> (CobolProgram) o.getDetail();
            case AnalysisTaskResultError e -> throw new RuntimeException(e.getException());
        };

        new InjectProgramDependenciesIntoNeo4JTask(new NodeSpecBuilder(new NamespaceQualifier("DEP-GRAPH")),
                new GraphSDK(new Neo4JDriverBuilder().fromEnv())).run(root);

    }
}
