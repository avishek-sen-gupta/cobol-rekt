package org.smojol.analysis.visualisation;

import com.google.common.collect.ImmutableList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.smojol.analysis.LanguageDialect;
import org.smojol.flowchart.FlowchartTasks;
import org.smojol.interpreter.FlowchartGenerationStrategy;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class FlowchartBuildMain {
    public static void main(String[] args) throws IOException, InterruptedException {
//        String sourceDir = "/Users/asgupta/code/smojol/smojol-test-code";
        String sourceDir = "/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cbl";
//        File[] copyBookPaths = new File[]{new File("/Users/asgupta/code/smojol/smojol-test-code")};
        File[] copyBookPaths = new File[]{new File("/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy")};
        String dialectJarPath = "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar";
        String reportRootDir = "/Users/asgupta/code/smojol/out/report";

        List<String> programNames = ImmutableList.of("CBACT01C.cbl");
        new FlowchartTasks(sourceDir, reportRootDir, copyBookPaths, dialectJarPath).generateForPrograms(programNames, FlowchartGenerationStrategy.FULL_PROGRAM, LanguageDialect.COBOL);
    }
}
