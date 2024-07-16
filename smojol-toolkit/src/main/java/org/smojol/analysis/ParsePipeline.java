package org.smojol.analysis;

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
import org.eclipse.lsp.cobol.common.ResultWithErrors;
import org.eclipse.lsp.cobol.common.benchmark.BenchmarkService;
import org.eclipse.lsp.cobol.common.benchmark.BenchmarkSession;
import org.eclipse.lsp.cobol.common.benchmark.Measurement;
import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.eclipse.lsp.cobol.common.mapping.ExtendedDocument;
import org.eclipse.lsp.cobol.common.mapping.ExtendedText;
import org.eclipse.lsp.cobol.common.message.MessageService;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.core.engine.analysis.AnalysisContext;
import org.eclipse.lsp.cobol.core.engine.dialects.DialectService;
import org.eclipse.lsp.cobol.core.engine.errors.ErrorFinalizerService;
import org.eclipse.lsp.cobol.core.engine.pipeline.Pipeline;
import org.eclipse.lsp.cobol.core.engine.pipeline.PipelineResult;
import org.eclipse.lsp.cobol.core.engine.pipeline.StageResult;
import org.eclipse.lsp.cobol.core.engine.pipeline.stages.*;
import org.eclipse.lsp.cobol.core.preprocessor.TextPreprocessor;
import org.eclipse.lsp.cobol.core.preprocessor.delegates.GrammarPreprocessor;
import org.smojol.analysis.visualisation.ComponentsBuilder;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.common.idms.DialectIntegratorListener;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.interpreter.SourceConfig;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

/**
 * The parse pipeline extracts out the functionality of the parse pipeline into an independent
 * reuseable component
 */
@Slf4j
public class ParsePipeline {
    private final File src;
    private final File[] cpyPaths;
    private final String[] cpyExt;
    private final String cobolParseTreeOutputPath;
    private final String dialectJarPath;
    private final ComponentsBuilder ops;
    private final LanguageDialect dialect;
    @Getter private CobolEntityNavigator navigator;
    @Getter private CobolDataStructure dataStructures;

    public ParsePipeline(SourceConfig sourceConfig, ComponentsBuilder ops, LanguageDialect dialect) {
        this.src = sourceConfig.source();
        this.cpyPaths = sourceConfig.copyBookPaths();
        this.cobolParseTreeOutputPath = sourceConfig.cobolParseTreeOutputPath();
        this.ops = ops;
        this.dialect = dialect;
        cpyExt = new String[]{"", ".cpy"};
        this.dialectJarPath = sourceConfig.dialectJarPath();
    }

    /**
     * Parses and returns a navigator to navigate the tree with
     * @return CobolEntityNavigator
     * @throws IOException
     */
    public CobolEntityNavigator parse() throws IOException {
        Injector diCtx = Guice.createInjector(new CliModule());
        Pipeline pipeline = setupPipeline(diCtx);

        CliClientProvider cliClientProvider = diCtx.getInstance(CliClientProvider.class);
        if (cpyPaths != null) {
            cliClientProvider.setCpyPaths(Arrays.asList(cpyPaths));
        }
        cliClientProvider.setCpyExt(Arrays.asList(cpyExt));

        // Cleaning up
        TextPreprocessor preprocessor = diCtx.getInstance(TextPreprocessor.class);
        BenchmarkService benchmarkService = diCtx.getInstance(BenchmarkService.class);
        ErrorFinalizerService errorFinalizerService = diCtx.getInstance(ErrorFinalizerService.class);
        if (src == null) {
            LOG.error("src must be provided");
            throw new RuntimeException("src must be provided");
        }

        if (cobolParseTreeOutputPath == null) {
            LOG.error("cobolParseTreeOutputPath must be provided");
            throw new RuntimeException("cobolParseTreeOutputPath must be provided");
        }

        String documentUri = src.toURI().toString();
        String text = new String(Files.readAllBytes(src.toPath()));

        ResultWithErrors<ExtendedText> resultWithErrors = preprocessor.cleanUpCode(documentUri, text);
        AnalysisContext ctx =
                new AnalysisContext(
                        new ExtendedDocument(resultWithErrors.getResult(), text),
                        dialect.analysisConfig(dialectJarPath),
                        benchmarkService.startSession());
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
            throw new RuntimeException("There were parsing errors. Debug and see why.");
        }

        ParserRuleContext tree = lastStageResult.getData().getTree();
        ParseTreeWalker walker = new ParseTreeWalker();
        EntityNavigatorBuilder navigatorBuilder = ops.getCobolEntityNavigatorBuilder();
        verifyNoNullDialectStatements(tree, navigatorBuilder);
        DialectIntegratorListener dialectIntegrationListener = new DialectIntegratorListener();
        walker.walk(dialectIntegrationListener, tree);
        System.out.println("[INFO] Restored " + dialectIntegrationListener.getRestores() + " nodes.");
        System.out.println("Building tree...");
        System.out.println("Built tree");

        // TODO: The navigator itself can probably determine these things,
        navigator = navigatorBuilder.navigator(tree);
        dataStructures = ops.getDataStructureBuilder(navigator).build();

        ops.getVisualiser().writeCobolAST(tree, cobolParseTreeOutputPath, false, navigator);

        JsonArray diagnostics = new JsonArray();
        ctx.getAccumulatedErrors()
                .forEach(
                        err -> {
                            JsonObject diagnostic = toJson(err, gson);
                            diagnostics.add(diagnostic);
                        });
        //        result.add("diagnostics", diagnostics);
        System.out.println(gson.toJson(timingResult));
        return navigator;
    }

    private static void verifyNoNullDialectStatements(ParserRuleContext tree, EntityNavigatorBuilder navigatorBuilder) {
        CobolEntityNavigator navigator = navigatorBuilder.navigator(tree);
        List<ParseTree> nullDialectStatements = navigator.findAllByCondition(n -> n.getClass() == CobolParser.DialectStatementContext.class
                && ((CobolParser.DialectStatementContext) n).dialectNodeFiller() != null
                && ((CobolParser.DialectStatementContext) n).dialectNodeFiller().whatever() != null, tree);
        nullDialectStatements.forEach(n -> {
            boolean removed = ((ParserRuleContext) n.getParent()).children.remove(n);
            System.out.println(removed ? "removed" : "not removed");
        });
        List<ParseTree> nullIdmsNodes = navigator.findAllByCondition(n -> n.getClass() == CobolParser.DialectNodeFillerContext.class
                && ((CobolParser.DialectNodeFillerContext) n).whatever() != null, tree);

        if (!nullIdmsNodes.isEmpty()) throw new RuntimeException("Null IDMS nodes detected, please run preprocess()");
    }

    /**
     * Returns the builder which will create the flow tree
     * @return FlowchartBuilder
     */
    public FlowchartBuilder flowcharter() {
        return ops.getFlowchartBuilderFactory().apply(navigator, dataStructures);
    }

    private static Pipeline setupPipeline(Injector diCtx) {
        DialectService dialectService = diCtx.getInstance(DialectService.class);
        MessageService messageService = diCtx.getInstance(MessageService.class);
        GrammarPreprocessor grammarPreprocessor = diCtx.getInstance(GrammarPreprocessor.class);
        ParseTreeListener parseTreeListener = diCtx.getInstance(ParseTreeListener.class);

        Pipeline pipeline = new Pipeline();
        pipeline.add(new DialectCompilerDirectiveStage(dialectService));
        pipeline.add(new CompilerDirectivesStage(messageService));
        pipeline.add(new DialectProcessingStage(dialectService));
        pipeline.add(new PreprocessorStage(grammarPreprocessor));
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
