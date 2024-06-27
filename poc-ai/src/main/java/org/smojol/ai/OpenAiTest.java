package org.smojol.ai;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.ast.FlowchartBuilderImpl;
import org.smojol.analysis.ParsePipeline;
import org.smojol.analysis.visualisation.CobolTreeVisualiserImpl;
import org.smojol.analysis.visualisation.PocOpsImpl;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.interpreter.navigation.CobolEntityNavigatorBuilderImpl;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.common.flowchart.NodeText;
import org.smojol.common.vm.strategy.UnresolvedReferenceThrowStrategy;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class OpenAiTest {
    public static void main(String[] args) throws IOException, InterruptedException {
        System.out.println("Working");
        Advisor advisor = new Advisor(System.getenv(Advisor.AZURE_OPENAI_API_KEY), System.getenv(Advisor.AZURE_OPENAI_ENDPOINT));

        String dotFilePath = "/Users/asgupta/Downloads/mbrdi-poc/flowchart.dot";
        String graphOutputPath = "/Users/asgupta/Downloads/mbrdi-poc/flowchart.png";
        String cobolParseTreeOutputPath = "/Users/asgupta/Downloads/mbrdi-poc/test-cobol.json";
        String idmsParseTreeOutputPath = "/Users/asgupta/Downloads/mbrdi-poc/test-idms.json";
        File[] copyBookPaths = {new File("/Users/asgupta/Downloads/mbrdi-poc")};
        File source = new File("/Users/asgupta/Downloads/mbrdi-poc/V75234");

        PocOpsImpl ops = new PocOpsImpl(new CobolTreeVisualiserImpl(),
                FlowchartBuilderImpl::build, new CobolEntityNavigatorBuilderImpl(), new UnresolvedReferenceThrowStrategy());
        String dialectJarPath = "/Users/asgupta/code/mbrdi-proleap/che4z/che-che4z-lsp-for-cobol-2.1.2/server/dialect-idms/target/dialect-idms.jar";
        ParsePipeline pipeline = new ParsePipeline(source,
                copyBookPaths,
                dialectJarPath, cobolParseTreeOutputPath,
                ops);

        CobolEntityNavigator navigator = pipeline.parse();
        FlowchartBuilder flowcharter = pipeline.flowcharter();
        ParseTree u204 = navigator.target("U204-CALL-COST-PRICE");
//        ParseTree u204 = navigator.target("U204B");
        String codeText = NodeText.originalText(u204, navigator::dialectText);
        System.out.println(codeText);

        PromptConstructor promptConstructor = new PromptConstructor();
        promptConstructor.addLine(RequestsResponses.PROMPT);
        promptConstructor.addLine(codeText);
        System.out.println("Processing your request...");
        List<String> responses = advisor.advise(promptConstructor.getPrompt());

        AiInterpreter interpreter = new AiInterpreter(pipeline, u204);
        interpreter.extractReferences(responses);
        interpreter.buildFlowchart();
        interpreter.write(dotFilePath, graphOutputPath);
        System.out.println("Complete");
    }
}
