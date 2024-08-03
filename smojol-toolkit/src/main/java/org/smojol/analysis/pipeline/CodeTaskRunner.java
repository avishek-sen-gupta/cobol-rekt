package org.smojol.analysis.pipeline;

import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import lombok.Getter;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.eclipse.lsp.cobol.core.CobolLexer;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.core.CobolSentenceParser;
import org.smojol.analysis.ParseDiagnosticRuntimeError;
import org.smojol.analysis.LanguageDialect;
import org.smojol.analysis.ParsePipeline;
import org.smojol.analysis.graph.neo4j.NodeReferenceStrategy;
import org.smojol.analysis.visualisation.ComponentsBuilder;
import org.smojol.ast.FlowchartBuilderImpl;
import org.smojol.ast.GenericProcessingFlowNode;
import org.smojol.common.ast.CobolTreeVisualiser;
import org.smojol.common.ast.CommentBlock;
import org.smojol.common.ast.NodeText;
import org.smojol.common.id.IdProvider;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.vm.strategy.UnresolvedReferenceThrowStrategy;
import org.smojol.interpreter.*;
import org.smojol.interpreter.structure.DefaultFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

public class CodeTaskRunner {
    private static final String AST_DIR = "ast";
    private static final String FLOW_AST_DIR = "flow_ast";
    private static final String IMAGES_DIR = "flowcharts";
    private static final String DOTFILES_DIR = "dotfiles";
    private static final String GRAPHML_DIR = "graphml";
    private static final String CFG_DIR = "cfg";
    private final String sourceDir;
    private final List<File> copyBookPaths;
    private final String dialectJarPath;
    private final String reportRootDir;
    private final LanguageDialect dialect;
    private final FlowchartGenerationStrategy flowchartGenerationStrategy;
    private final IdProvider idProvider;
    @Getter
    private final Map<String, List<SyntaxError>> errorMap = new HashMap<>();

    public CodeTaskRunner(String sourceDir, String reportRootDir, List<File> copyBookPaths, String dialectJarPath, LanguageDialect dialect, FlowchartGenerationStrategy flowchartGenerationStrategy, IdProvider idProvider) {
        this.sourceDir = sourceDir;
        this.copyBookPaths = copyBookPaths;
        this.dialectJarPath = dialectJarPath;
        this.reportRootDir = reportRootDir;
        this.dialect = dialect;
        this.flowchartGenerationStrategy = flowchartGenerationStrategy;
        this.idProvider = idProvider;
        reportParameters();
    }

    private void reportParameters() {
        System.out.println("Parameters passed in \n--------------------");
        System.out.println("srcDir = " + sourceDir);
        System.out.println("reportRootDir = " + reportRootDir);
        System.out.println("dialectJarPath = " + dialectJarPath);
        System.out.println("copyBookPaths = " + String.join(",", copyBookPaths.stream().map(cp -> cp.toString() + "\n").toList()));
    }

    public CodeTaskRunner generateForPrograms(List<AnalysisTask> tasks, List<String> programFilenames, TaskRunnerMode runnerMode) throws IOException {
        for (String programFilename : programFilenames) {
            System.out.println(String.format("Running tasks: %s for program '%s' in %s mode...",
                    tasks.stream().map(AnalysisTask::name).toList(),
                    programFilename, runnerMode.toString()));
            try {
                generateForProgram(programFilename, sourceDir, reportRootDir, this.dialect, runnerMode.tasks(tasks));
            } catch (ParseDiagnosticRuntimeError e) {
                errorMap.put(programFilename, e.getErrors());
            }
        }

        return runnerMode.run(errorMap, this);
    }

    public CodeTaskRunner generateForPrograms(List<AnalysisTask> tasks, List<String> programFilenames) throws IOException {
        return generateForPrograms(tasks, programFilenames, TaskRunnerMode.PRODUCTION_MODE);
    }

    private void generateForProgram(String programFilename, String sourceDir, String reportRootDir, LanguageDialect dialect, List<AnalysisTask> tasks) throws IOException {
        File source = Paths.get(sourceDir, programFilename).toFile().getAbsoluteFile();
        String programReportDir = String.format("%s.report", programFilename);
        Path astOutputDir = Paths.get(reportRootDir, programReportDir, AST_DIR).toAbsolutePath().normalize();
        Path flowASTOutputDir = Paths.get(reportRootDir, programReportDir, FLOW_AST_DIR).toAbsolutePath().normalize();
        Path imageOutputDir = Paths.get(reportRootDir, programReportDir, IMAGES_DIR).toAbsolutePath().normalize();
        Path dotFileOutputDir = Paths.get(reportRootDir, programReportDir, DOTFILES_DIR).toAbsolutePath().normalize();
        Path graphMLExportOutputDir = Paths.get(reportRootDir, programReportDir, GRAPHML_DIR).toAbsolutePath().normalize();
        String graphMLExportOutputPath = graphMLExportOutputDir.resolve(String.format("%s.graphml", programFilename)).toAbsolutePath().normalize().toString();
        Path cfgOutputDir = Paths.get(reportRootDir, programReportDir, CFG_DIR).toAbsolutePath().normalize();
        String cfgOutputPath = cfgOutputDir.resolve(String.format("cfg-%s.json", programFilename)).toAbsolutePath().normalize().toString();
        String cobolParseTreeOutputPath = astOutputDir.resolve(String.format("cobol-%s.json", programFilename)).toAbsolutePath().normalize().toString();
        String flowASTOutputPath = flowASTOutputDir.resolve(String.format("flow-ast-%s.json", programFilename)).toAbsolutePath().normalize().toString();
        String absoluteDialectJarPath = Paths.get(dialectJarPath).toAbsolutePath().normalize().toString();
        SourceConfig sourceConfig = new SourceConfig(programFilename, source, copyBookPaths, cobolParseTreeOutputPath, absoluteDialectJarPath);

        FlowchartOutputWriter flowchartOutputWriter = new FlowchartOutputWriter(flowchartGenerationStrategy, dotFileOutputDir, imageOutputDir);
        RawASTOutputConfig rawAstOutputConfig = new RawASTOutputConfig(astOutputDir, new CobolTreeVisualiser());
        FlowASTOutputConfig flowASTOutputConfig = new FlowASTOutputConfig(flowASTOutputDir, flowASTOutputPath);
        GraphMLExportConfig graphMLOutputConfig = new GraphMLExportConfig(graphMLExportOutputDir, graphMLExportOutputPath);
        CFGOutputConfig cfgOutputConfig = new CFGOutputConfig(cfgOutputDir, cfgOutputPath);
        GraphSDK sdk = new GraphSDK(new Neo4JDriverBuilder().fromEnv());
        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiser(),
                FlowchartBuilderImpl::build, new EntityNavigatorBuilder(), new UnresolvedReferenceThrowStrategy(),
                new DefaultFormat1DataStructureBuilder(), idProvider);
        ParsePipeline pipeline = new ParsePipeline(sourceConfig, ops, dialect);
        GraphBuildConfig graphBuildConfig = new GraphBuildConfig(
                NodeReferenceStrategy.EXISTING_CFG_NODE,
                NodeReferenceStrategy.EXISTING_CFG_NODE);

        SmojolTasks pipelineTasks = new SmojolTasks(pipeline,
                sourceConfig, flowchartOutputWriter,
                rawAstOutputConfig, graphMLOutputConfig,
                flowASTOutputConfig, cfgOutputConfig,
                graphBuildConfig, sdk,
                idProvider).build();
        List<CommentBlock> commentBlocks = extractComments(programFilename, sourceDir, pipeline.getNavigator());
        pipelineTasks.run(tasks);
    }

    private static List<CommentBlock> extractComments(String programFilename, String sourceDir, CobolEntityNavigator navigator) throws IOException {
        List<CommentBlock> allCommentBlocks = new ArrayList<>();
        CommentBlock currentBlock = new CommentBlock();
        List<String> lines = Files.readAllLines(Paths.get(sourceDir, programFilename), StandardCharsets.ISO_8859_1)
                .stream().filter(l -> l.trim().length() > 7).toList();
        List<String> linesWithoutAreaA = lines.stream().map(l -> l.substring(6)).toList();
        for (String line : linesWithoutAreaA) {
            if (line.startsWith("*")) {
                if (!containsWords(line) || validCobol(line.substring(1))) continue;
                if (currentBlock == null) currentBlock = new CommentBlock();
                currentBlock.add(line);
            } else {
                if (currentBlock == null) continue;
                currentBlock.setCodeContext(line);
                allCommentBlocks.add(currentBlock);
                currentBlock = null;
            }
        }

        allCommentBlocks.forEach(block -> {
            ParseTree matchingNode = navigator.findByCondition(n -> NodeText.originalText(n, NodeText::PASSTHROUGH).contains(block.getCodeContextLine()));
            block.setAssociatedTree(matchingNode);
        });
        return allCommentBlocks;
    }

    private static boolean containsWords(String line) {
        return line.chars().mapToObj(c -> Character.isAlphabetic(c) || Character.isDigit(c)).reduce(false, (b, c) -> b || c);
    }

    private static boolean validCobol(String line) {
        CobolLexer antlrLexer = new CobolLexer(CharStreams.fromString(line));
        antlrLexer.removeErrorListeners();
        CommonTokenStream tokens = new CommonTokenStream(antlrLexer);
        CobolSentenceParser antlrParser = new CobolSentenceParser(tokens);
        antlrParser.removeErrorListeners();
        SentenceParserErrorListener listener = new SentenceParserErrorListener();
        antlrParser.addErrorListener(listener);
        CobolSentenceParser.StartRuleContext startRuleContext = antlrParser.startRule();
        return !listener.hasErrors();
    }
}

class SentenceParserErrorListener implements ANTLRErrorListener {
    private final List<RecognitionException> exceptions = new ArrayList<>();

    @Override
    public void syntaxError(Recognizer<?, ?> recognizer, Object o, int i, int i1, String s, RecognitionException e) {
        exceptions.add(e);
    }

    @Override
    public void reportAmbiguity(Parser parser, DFA dfa, int i, int i1, boolean b, BitSet bitSet, ATNConfigSet atnConfigSet) {

    }

    @Override
    public void reportAttemptingFullContext(Parser parser, DFA dfa, int i, int i1, BitSet bitSet, ATNConfigSet atnConfigSet) {

    }

    @Override
    public void reportContextSensitivity(Parser parser, DFA dfa, int i, int i1, int i2, ATNConfigSet atnConfigSet) {

    }

    public boolean hasErrors() {
        return !exceptions.isEmpty();
    }
}
