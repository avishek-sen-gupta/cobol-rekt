package org.smojol.analysis.visualisation;

import com.google.common.collect.ImmutableList;
import org.smojol.analysis.pipeline.AnalyseDependenciesTask;

import java.io.File;
import java.io.IOException;

public class DependencyBuildMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        CobolProgram rootProgram = new AnalyseDependenciesTask("/Users/asgupta/code/smojol/smojol-test-code",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/out/report",
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar")
                .run("if-test.cbl");
        System.out.println(rootProgram);
    }
}
