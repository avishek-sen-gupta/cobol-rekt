package org.smojol.api;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import io.javalin.Javalin;
import io.javalin.json.JsonMapper;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.jooq.Record2;
import org.jooq.Result;
import org.smojol.api.contract.ProjectListing;
import org.smojol.api.database.DbContext;
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
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Logger;

import static org.smojol.toolkit.task.CommandLineAnalysisTask.BUILD_BASE_ANALYSIS;

public class ApiServer {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(ApiServer.class.getName());

    public void runServer(int port, JsonMapper gsonMapper, Gson gson, DbContext dbContext) {
        Javalin.create(config -> {
                    config.jsonMapper(gsonMapper);
                    config.staticFiles.add("/static/dist");
                    config.requestLogger.http((ctx, ms) -> LOGGER.info("Got a request: " + ctx.path()));
                })
                .get("/api/heartbeat", ctx -> ctx.result("Hello World!"))
                .get("/api/flowchart/{id}", ctx -> {
                    Optional<Map<String, Object>> flowModel = flowchart(ctx.pathParam("id"), gson, dbContext);
                    if (flowModel.isEmpty()) {
                        ctx.status(404);
                        return;
                    }
                    ctx.json(flowModel.get());
                })
                .get("/api/flow-model/{id}", ctx -> {
                    Optional<Map<String, Object>> flowModel = flowModel(ctx.pathParam("id"), gson, dbContext);
                    if (flowModel.isEmpty()) {
                        ctx.status(404);
                        return;
                    }
                    ctx.json(flowModel.get());
                })
                .get("/api/ir-ast/{id}", ctx -> {
                    Optional<Map<String, Object>> ast = irAST(ctx.pathParam("id"), gson, dbContext);
                    if (ast.isEmpty()) {
                        ctx.status(404);
                        return;
                    }
                    ctx.json(ast.get());
                })
                .get("/api/ir-cfg/{id}", ctx -> {
                    Optional<Map<String, Object>> cfg = irCFG(ctx.pathParam("id"), gson, dbContext);
                    if (cfg.isEmpty()) {
                        ctx.status(404);
                        return;
                    }
                    ctx.json(cfg.get());
                })
                .get("/api/ir-cfg/{id}/loop-body", ctx -> {
                    ctx.json(loopBodies(ctx.pathParam("id"), gson, dbContext));
                })
                .get("/api/ir-cfg/{id}/t1-t2", ctx -> {
                    Optional<Boolean> isReducible = t1t2Analysis(ctx.pathParam("id"), gson, dbContext);
                    if (isReducible.isEmpty()) ctx.status(404);
                    ctx.json(ImmutableMap.of("isReducible", isReducible.get()));
                })
                .get("/api/projects", ctx -> ctx.json(projectListings(gson, dbContext)))
                .start(port);
    }

    private static List<ProjectListing> projectListings(Gson gson, DbContext dbContext) throws SQLException {
        return dbContext.execute(using -> new IntermediateFormService(gson).allProjectEntities(using));
    }

    private static Optional<Map<String, Object>> flowModel(String id, Gson gson, DbContext dbContext) throws SQLException {
        return dbContext.execute(using -> new SourceService(gson).flowModel(Integer.parseInt(id), using));
    }

    private static Optional<Map<String, Object>> flowchart(String id, Gson gson, DbContext dbContext) throws SQLException {
        return dbContext.execute(using -> new SourceService(gson).flowchart(Integer.parseInt(id), using));
    }

    private static Optional<Map<String, Object>> irAST(String id, Gson gson, DbContext dbContext) throws SQLException {
        return dbContext.execute(using -> new IntermediateFormService(gson).intermediateAST(Integer.parseInt(id), using));
    }

    private static Optional<Map<String, Object>> irCFG(String id, Gson gson, DbContext dbContext) throws SQLException {
        return dbContext.execute(using -> new IntermediateFormService(gson).intermediateCFG(Integer.parseInt(id), using));
    }

    private static List<JsonObject> loopBodies(String id, Gson gson, DbContext dbContext) throws SQLException {
        Result<Record2<Long, String>> loopBodiesFromDB = dbContext.execute(using -> new IntermediateFormService(gson).loopBodiesByCfgID(Long.parseLong(id), using));
        return loopBodiesFromDB.map(rec -> gson.fromJson(rec.component2(), JsonObject.class));
    }

    private static Optional<Boolean> t1t2Analysis(String id, Gson gson, DbContext dbContext) throws SQLException {
        Optional<Boolean> t1t2Result = dbContext.execute(using -> new IntermediateFormService(gson).getT1T2AnalysisResult(Long.parseLong(id), using));
        return t1t2Result;
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
