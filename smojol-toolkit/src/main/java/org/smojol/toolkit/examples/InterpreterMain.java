package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.toolkit.analysis.defined.InterpretTask;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.smojol.toolkit.interpreter.interpreter.CobolConditionResolver;
import org.smojol.toolkit.task.AnalysisTaskResult;

import java.io.File;
import java.io.IOException;

public class InterpreterMain {
    private final Logger logger = LoggerFactory.getLogger(InterpreterMain.class);

    public static void main(String[] args) throws IOException {
        SourceConfig testSourceConfig = new SourceConfig(
                "test-exp.cbl", "/Users/asgupta/code/smojol/smojol-test-code",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar");
//        File source = new File("/Users/asgupta/code/smojol/smojol-test-code/table-indexing.cbl");
//        File source = new File("/Users/asgupta/code/smojol/smojol-test-code/table-redef.cbl");
//        File source = new File("/Users/asgupta/code/smojol/smojol-test-code/simple-redef.cbl");

        SourceConfig awsCardDemoConfig = new SourceConfig(
                "CBACT01C.cbl", "/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cbl",
                ImmutableList.of(new File("/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar");

        AnalysisTaskResult result = new InterpretTask(awsCardDemoConfig, LanguageDialect.COBOL, CobolConditionResolver.ALWAYS_YES, new LocalFilesystemOperations()).run();
        System.out.println(result);
    }
}
