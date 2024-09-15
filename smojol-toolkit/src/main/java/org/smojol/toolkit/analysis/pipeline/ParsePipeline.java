package org.smojol.toolkit.analysis.pipeline;

import com.google.gson.*;
import com.google.inject.Guice;
import com.google.inject.Injector;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeListener;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.eclipse.lsp.cobol.cli.di.CliModule;
import org.eclipse.lsp.cobol.cli.modules.CliClientProvider;
import org.eclipse.lsp.cobol.common.CleanerPreprocessor;
import org.eclipse.lsp.cobol.common.ResultWithErrors;
import org.eclipse.lsp.cobol.common.benchmark.BenchmarkService;
import org.eclipse.lsp.cobol.common.benchmark.BenchmarkSession;
import org.eclipse.lsp.cobol.common.benchmark.Measurement;
import org.eclipse.lsp.cobol.common.dialects.CobolLanguageId;
import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.eclipse.lsp.cobol.common.mapping.ExtendedDocument;
import org.eclipse.lsp.cobol.common.mapping.ExtendedText;
import org.eclipse.lsp.cobol.common.message.MessageService;
import org.eclipse.lsp.cobol.common.pipeline.Pipeline;
import org.eclipse.lsp.cobol.common.pipeline.PipelineResult;
import org.eclipse.lsp.cobol.common.pipeline.StageResult;
import org.eclipse.lsp.cobol.core.engine.analysis.AnalysisContext;
import org.eclipse.lsp.cobol.core.engine.dialects.DialectService;
import org.eclipse.lsp.cobol.core.engine.errors.ErrorFinalizerService;
import org.eclipse.lsp.cobol.core.preprocessor.delegates.GrammarPreprocessor;
import org.eclipse.lsp.cobol.dialects.TrueDialectServiceImpl;
import org.eclipse.lsp.cobol.dialects.ibm.*;
import org.smojol.common.dependency.ComponentsBuilder;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.common.idms.DialectIntegratorListener;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.error.ParseDiagnosticRuntimeError;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
import org.smojol.toolkit.analysis.validation.DataStructureValidation;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.logging.Logger;

/**
 * The parse pipeline extracts out the functionality of the parse pipeline into an independent
 * reuseable component
 */
@Slf4j
public class ParsePipeline {
    private static final Logger LOGGER = Logger.getLogger(ParsePipeline.class.getName());
    private final File src;
    private final List<File> cpyPaths;
    private final String[] cpyExt;
    private final String dialectJarPath;
    private final ComponentsBuilder ops;
    private final LanguageDialect dialect;
    @Getter private CobolEntityNavigator navigator;
    @Getter private CobolDataStructure dataStructures;
    @Getter private ParserRuleContext tree;
    @Getter private List<ParseTree> transfersOfControl;
    @Getter private List<ParseTree> subroutineCalls;

    public ParsePipeline(SourceConfig sourceConfig, ComponentsBuilder ops, LanguageDialect dialect) {
        this.src = sourceConfig.source();
        this.cpyPaths = sourceConfig.copyBookPaths();
        this.ops = ops;
        this.dialect = dialect;
        cpyExt = new String[]{"", ".cpy"};
        this.dialectJarPath = sourceConfig.dialectJarPath();
    }

    public CobolEntityNavigator parse() throws IOException {
        return parse(DataStructureValidation.BUILD);
    }
    /**
     * Parses and returns a navigator to navigate the tree with
     * @return CobolEntityNavigator
     * @throws IOException
     */
    public CobolEntityNavigator parse(DataStructureValidation dataStructureValidation) throws IOException {
        Injector diCtx = Guice.createInjector(new CliModule());

        CliClientProvider cliClientProvider = diCtx.getInstance(CliClientProvider.class);
        if (cpyPaths != null) {
            cliClientProvider.setCpyPaths(cpyPaths);
        }
        cliClientProvider.setCpyExt(Arrays.asList(cpyExt));

        CleanerPreprocessor preprocessor = diCtx.getInstance(TrueDialectServiceImpl.class).getPreprocessor(CobolLanguageId.COBOL);
        Pipeline pipeline = setupPipeline(diCtx, preprocessor);
        BenchmarkService benchmarkService = diCtx.getInstance(BenchmarkService.class);
        ErrorFinalizerService errorFinalizerService = diCtx.getInstance(ErrorFinalizerService.class);
        if (src == null) {
            LOG.error("src must be provided");
            throw new RuntimeException("src must be provided");
        }

        String documentUri = src.toURI().toString();
//        String text = new String(Files.readAllBytes(src.toPath()));
        String text = new String(ops.getResourceOperations().readAllBytes(src.toPath()));

        ResultWithErrors<ExtendedText> resultWithErrors = preprocessor.cleanUpCode(documentUri, text);
        AnalysisContext ctx =
                new AnalysisContext(
                        new ExtendedDocument(resultWithErrors.getResult(), text),
                        dialect.analysisConfig(dialectJarPath),
                        benchmarkService.startSession(), src.toURI().toString(), text, CobolLanguageId.COBOL);
        ctx.getAccumulatedErrors().addAll(resultWithErrors.getErrors());
        PipelineResult pipelineResult = pipeline.run(ctx);
        Gson gson = new GsonBuilder().setPrettyPrinting().addSerializationExclusionStrategy(new ExclusionStrategy() {
            @Override
            public boolean shouldSkipField(FieldAttributes fieldAttributes) {
                return "recognizer".equals(fieldAttributes.getName()) || "children".equals(fieldAttributes.getName())
                        || "exception".equals(fieldAttributes.getName())
                        || "ctx".equals(fieldAttributes.getName())
                        || "contextUsages".equals(fieldAttributes.getName());
            }

            @Override
            public boolean shouldSkipClass(Class<?> aClass) {
                return RecognitionException.class.equals(aClass);
            }
        }).create();
        JsonObject timingResult = new JsonObject();
        addTiming(timingResult, ctx.getBenchmarkSession());

        StageResult<ParserStageResult> lastStageResult = (StageResult<ParserStageResult>) pipelineResult.getLastStageResult();
        errorFinalizerService.processLateErrors(ctx, ctx.getCopybooksRepository());

        if (!ctx.getAccumulatedErrors().isEmpty()) {
            ctx.getAccumulatedErrors().forEach(e -> LOGGER.info(e.toString()));
            throw new ParseDiagnosticRuntimeError("There were parsing errors!", ctx.getAccumulatedErrors());
        }

        tree = lastStageResult.getData().getTree();
        ParseTreeWalker walker = new ParseTreeWalker();
        EntityNavigatorBuilder navigatorBuilder = ops.getCobolEntityNavigatorBuilder();
        dialect.verifyNoNullDialectStatements(tree, navigatorBuilder);
        DialectIntegratorListener dialectIntegrationListener = new DialectIntegratorListener();
        walker.walk(dialectIntegrationListener, tree);
        LOGGER.info("Restored " + dialectIntegrationListener.getRestores() + " nodes.");
        LOGGER.info("Building tree...");
        LOGGER.info("Built tree");

        // TODO: The navigator itself can probably determine these things,
        navigator = navigatorBuilder.navigator(tree);
        dataStructures = dataStructureValidation.run(ops.getDataStructureBuilder(navigator));
        LOGGER.info(gson.toJson(timingResult));
        return navigator;
    }

    /**
     * Returns the builder which will create the flow tree
     * @return FlowchartBuilder
     */
    public FlowchartBuilder flowcharter() {
        return ops.getFlowchartBuilderFactory().apply(navigator, dataStructures, ops.getIdProvider());
    }

    private static Pipeline setupPipeline(Injector diCtx, CleanerPreprocessor preprocessor) {
        DialectService dialectService = diCtx.getInstance(DialectService.class);
        MessageService messageService = diCtx.getInstance(MessageService.class);
        GrammarPreprocessor grammarPreprocessor = diCtx.getInstance(GrammarPreprocessor.class);
        ParseTreeListener parseTreeListener = diCtx.getInstance(ParseTreeListener.class);

        Pipeline pipeline = new Pipeline();
        pipeline.add(new DialectCompilerDirectiveStage(dialectService));
        pipeline.add(new CompilerDirectivesStage(messageService));
        pipeline.add(new DialectProcessingStage(dialectService, preprocessor));
        pipeline.add(new PreprocessorStage(grammarPreprocessor, preprocessor));
        pipeline.add(new ImplicitDialectProcessingStage(dialectService));
        pipeline.add(new ParserStage(messageService, parseTreeListener));
        return pipeline;
    }

    private void addTiming(JsonObject result, BenchmarkSession benchmarkSession) {
        JsonObject tObj = new JsonObject();
        benchmarkSession.getMeasurements()
                .forEach(m -> tObj.add(m.getId(), new JsonPrimitive(m.getTime() / 1_000_000_000.0)));
        result.add("timings", tObj);
        benchmarkSession.getMeasurements().stream()
                .map(Measurement::getTime)
                .reduce(Long::sum)
                .ifPresent(totalTime -> tObj.add("total", new JsonPrimitive(totalTime / 1_000_000_000.0)));
    }

    private JsonObject toJson(SyntaxError syntaxError, Gson gson) {
        JsonObject diagnostic = new JsonObject();
        Optional.ofNullable(syntaxError.getErrorCode())
                .ifPresent(code -> diagnostic.add("code", new JsonPrimitive(code.getLabel())));
        Optional.ofNullable(syntaxError.getErrorSource())
                .ifPresent(es -> diagnostic.add("source", new JsonPrimitive(es.getText())));
        Optional.ofNullable(syntaxError.getLocation())
                .ifPresent(l -> diagnostic.add("location", gson.toJsonTree(l)));
        Optional.ofNullable(syntaxError.getSeverity())
                .ifPresent(s -> diagnostic.add("severity", new JsonPrimitive(s.name())));
        Optional.ofNullable(syntaxError.getSuggestion())
                .ifPresent(s -> diagnostic.add("suggestion", new JsonPrimitive(s)));
        Optional.ofNullable(syntaxError.getRelatedInformation())
                .ifPresent(ri -> diagnostic.add("related", gson.toJsonTree(ri)));
        return diagnostic;
    }
}
