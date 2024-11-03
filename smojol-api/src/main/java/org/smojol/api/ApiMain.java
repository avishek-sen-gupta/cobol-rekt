package org.smojol.api;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import io.javalin.Javalin;
import io.javalin.json.JsonMapper;
import org.jetbrains.annotations.NotNull;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.jooq.DSLContext;
import org.jooq.SQLDialect;
import org.jooq.impl.DSL;
import org.smojol.api.contract.ProjectListing;
import org.smojol.api.database.ConnectionBuilder;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.pseudocode.BasicBlock;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.transpiler.PruneUnreachableTask;
import org.smojol.common.transpiler.TranspilerFlowgraph;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Type;
import java.net.URISyntaxException;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.logging.Logger;

import static org.smojol.toolkit.task.CommandLineAnalysisTask.BUILD_BASE_ANALYSIS;

public class ApiMain {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(ApiMain.class.getName());

    public static void main(String[] args) throws IOException, URISyntaxException, SQLException {
        Gson gson = new GsonBuilder().create();
        JsonMapper gsonMapper = getJsonMapper(gson);

        ConnectionBuilder connectionBuilder = new ConnectionBuilder(System.getenv("DATABASE_URL"),
                System.getenv("DATABASE_USER"),
                System.getenv("DATABASE_PASSWORD"));
        runServer(7070, gsonMapper, connectionBuilder);
    }

    private static void runServer(int port, JsonMapper gsonMapper, ConnectionBuilder connectionBuilder) {
        Javalin.create(config -> {
                    config.jsonMapper(gsonMapper);
                    config.staticFiles.add("/static/dist");
                    config.requestLogger.http((ctx, ms) -> LOGGER.info("Got a request: " + ctx.path()));
                })
                .get("/api/heartbeat", ctx -> ctx.result("Hello World!"))
                .get("/api/ir-ast/{id}", ctx -> {
                    Optional<Map<String, Object>> ast = irAST(ctx.pathParam("id"), connectionBuilder);
                    if (ast.isEmpty()) {
                        ctx.status(404);
                        return;
                    }
                    ctx.json(ast.get());
                })
                .get("/api/ir-cfg/{id}", ctx -> {
                    Optional<Map<String, Object>> cfg = irCFG(ctx.pathParam("id"), connectionBuilder);
                    if (cfg.isEmpty()) {
                        ctx.status(404);
                        return;
                    }
                    ctx.json(cfg.get());
                })
                .get("/api/projects", ctx -> ctx.json(projectListings(connectionBuilder)))
                .start(port);
    }

    private static Optional<Map<String, Object>> irCFG(String id, ConnectionBuilder connectionBuilder) throws SQLException {
        try (Connection conn = connectionBuilder.getConnection()) {
            DSLContext using = DSL.using(conn, SQLDialect.SQLITE);
            return new IntermediateASTService().intermediateCFG(Integer.parseInt(id), using);
        }
    }

    private static @NotNull JsonMapper getJsonMapper(Gson gson) {
        JsonMapper gsonMapper = new JsonMapper() {
            @NotNull
            @Override
            public String toJsonString(@NotNull Object obj, @NotNull Type type) {
                String json = gson.toJson(obj, type);
                return json;
            }

            @NotNull
            @Override
            public <T> T fromJsonString(@NotNull String json, @NotNull Type targetType) {
                return gson.fromJson(json, targetType);
            }
        };
        return gsonMapper;
    }

    private static List<ProjectListing> projectListings(ConnectionBuilder connectionBuilder) throws SQLException {
        try (Connection conn = connectionBuilder.getConnection()) {
            DSLContext using = DSL.using(conn, SQLDialect.SQLITE);
            return new IntermediateASTService().allProjectEntities(using);
        }
    }

    private static Optional<Map<String, Object>> irAST(String id, ConnectionBuilder connectionBuilder) throws SQLException {
        try (Connection conn = connectionBuilder.getConnection()) {
            DSLContext using = DSL.using(conn, SQLDialect.SQLITE);
            return new IntermediateASTService().intermediateAST(Integer.parseInt(id), using);
        }
    }

    private static Graph<TranspilerInstruction, DefaultEdge> flowgraph() throws IOException {
        String programName = "test-exp.cbl";
        Map<String, List<AnalysisTaskResult>> analysisResult = new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.MERMAID), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(BUILD_BASE_ANALYSIS, CommandLineAnalysisTask.BUILD_TRANSPILER_FLOWGRAPH), ImmutableList.of(programName));
        List<AnalysisTaskResult> results = analysisResult.get(programName);
        TranspilerFlowgraph transpilerFlowgraph = ((AnalysisTaskResultOK) results.get(1)).getDetail();
        Graph<BasicBlock<TranspilerInstruction>, DefaultEdge> blockGraph = transpilerFlowgraph.basicBlockFlowgraph();
        Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph = transpilerFlowgraph.instructionFlowgraph();
        PruneUnreachableTask.pruneUnreachableInstructions(transpilerFlowgraph);
        return instructionFlowgraph;
    }
}
