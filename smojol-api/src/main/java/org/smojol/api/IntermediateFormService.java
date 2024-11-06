package org.smojol.api;

import com.google.common.collect.ImmutableMap;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.jooq.Record;
import org.jooq.*;
import org.smojol.api.contract.IntermediateASTListing;
import org.smojol.api.contract.IntermediateCFGListing;
import org.smojol.api.contract.ProjectListing;
import org.smojol.common.analysis.NaturalLoopBody;
import org.smojol.common.transpiler.FlowgraphReductionResult;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.common.transpiler.TranspilerNode;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.jooq.impl.DSL.field;
import static org.jooq.impl.DSL.table;

public class IntermediateFormService {
    public static final @NotNull Field<String> PROGRAM_NAME_FIELD = field("PROGRAM_NAME", String.class);
    public static final String PROJECT_ID = "PROJECT_ID";
    public static final @NotNull Field<Long> PROJECT_ID_FIELD = field(PROJECT_ID, Long.class);
    public static Field<Long> ID_FIELD = field("ID", Long.class);
    public static final @NotNull Field<Long> CFG_ID_FIELD = field("CFG_ID", Long.class);
    public static final @NotNull Field<String> BODY_FIELD = field("BODY", String.class);
    private final Gson gson;
    private static final @NotNull Table<Record> IR_AST = table("IR_AST");
    private static final @NotNull Table<Record> IR_CFG = table("IR_CFG");
    private static final @NotNull Table<Record> PROJECT = table("PROJECT");
    private static final @NotNull Table<Record> LOOP_BODY = table("LOOP_BODY");
    private static final @NotNull Table<Record> T1_T2_ANALYSIS = table("T1_T2_ANALYSIS");

    public IntermediateFormService(Gson gson) {
        this.gson = gson;
    }

    private Map<String, List<Map<String, String>>> intermediateASTListingsByProject(DSLContext using) {
        @NotNull Result<Record4<String, String, Long, Long>> allIntermediateASTs = using
                .select(field("IR_AST.PROGRAM_NAME", String.class),
                        field("IR_AST.IR_AST", String.class),
                        field("IR_AST.ID", Long.class),
                        field("PROJECT.ID", Long.class).as(PROJECT_ID))
                .from(IR_AST)
                .leftOuterJoin(PROJECT)
                .on(field("IR_AST.PROJECT_ID", Long.class)
                        .eq(field("PROJECT.ID", Long.class)))
                .fetch();
        Map<String, List<Map<String, String>>> collectedIntermediateASTs = allIntermediateASTs
                .map(ast -> (Map<String, String>) ImmutableMap.of("programName", ast.component1(), "astID", ast.component3().toString(), "projectID", ast.component4().toString())).stream()
                .collect(Collectors.groupingBy(d -> d.get("projectID"))).entrySet().stream()
                .collect(Collectors.toMap(Entry::getKey, Entry::getValue));
        return collectedIntermediateASTs;
    }

    Map<String, List<Map<String, String>>> intermediateCFGListingsByProject(DSLContext using) {
        @NotNull Result<Record4<String, String, Long, Long>> allIntermediateASTs = using
                .select(field("IR_CFG.PROGRAM_NAME", String.class),
                        field("IR_CFG.IR_CFG", String.class),
                        field("IR_CFG.ID", Long.class),
                        field("PROJECT.ID", Long.class).as(PROJECT_ID))
                .from(IR_CFG)
                .leftOuterJoin(PROJECT)
                .on(field("IR_CFG.PROJECT_ID", Long.class)
                        .eq(field("PROJECT.ID", Long.class)))
                .fetch();
        Map<String, List<Map<String, String>>> collectedIntermediateCFGs = allIntermediateASTs
                .map(cfg -> (Map<String, String>) ImmutableMap.of("programName", cfg.component1(), "cfgID", cfg.component3().toString(), "projectID", cfg.component4().toString())).stream()
                .collect(Collectors.groupingBy(d -> d.get("projectID"))).entrySet().stream()
                .collect(Collectors.toMap(Entry::getKey, Entry::getValue));
        return collectedIntermediateCFGs;
    }

    public List<ProjectListing> allProjectEntities(DSLContext using) {
        Map<String, List<Map<String, String>>> astGroupAsMap = intermediateASTListingsByProject(using);
        Map<String, List<Map<String, String>>> cfgGroupAsMap = intermediateCFGListingsByProject(using);
        Set<Entry<String, List<Map<String, String>>>> cfgsGroupedByProjectID = cfgGroupAsMap.entrySet();
        Set<Entry<String, List<Map<String, String>>>> astsGroupedByProjectID = astGroupAsMap.entrySet();
        Stream<String> cfgUniqueProjectIDs = cfgsGroupedByProjectID.stream().map(Entry::getKey);
        Stream<String> astUniqueProjectIDs = astsGroupedByProjectID.stream().map(Entry::getKey);
        Set<String> allUniqueProjectIDs = Stream.concat(cfgUniqueProjectIDs, astUniqueProjectIDs).collect(Collectors.toUnmodifiableSet());

        List<ProjectListing> projectListings = allUniqueProjectIDs.stream().map(pid -> new ProjectListing(pid,
                astGroupAsMap.get(pid).stream()
                        .map(ast -> new IntermediateASTListing(ast.get("astID"), ast.get("programName")))
                        .toList(),
                cfgGroupAsMap.get(pid).stream()
                        .map(cfg -> new IntermediateCFGListing(cfg.get("cfgID"), cfg.get("programName")))
                        .toList())).toList();

        return projectListings;
    }

    public Optional<Map<String, Object>> intermediateAST(long id, DSLContext using) {
        @NotNull Result<Record2<Long, String>> ast = using.select(ID_FIELD, field("IR_AST", String.class)).from(IR_AST).where(ID_FIELD.eq(id)).fetch();
        if (ast.isEmpty()) return Optional.empty();
        return Optional.of(ImmutableMap.of("id", id, "ast", gson.fromJson(ast.getFirst().component2(), JsonObject.class)));
    }

    public Optional<Map<String, Object>> intermediateCFG(long id, DSLContext using) {
        @NotNull Result<Record2<Long, String>> ast = using.select(ID_FIELD, field("IR_CFG", String.class))
                .from(IR_CFG)
                .where(ID_FIELD.eq(id))
                .fetch();
        if (ast.isEmpty()) return Optional.empty();
        return Optional.of(ImmutableMap.of("id", id, "cfg", gson.fromJson(ast.getFirst().component2(), JsonObject.class)));
    }

    public long insertIntermediateAST(TranspilerNode tree, String programName, long projectID, DSLContext using) {
        return using.insertInto(IR_AST)
                .columns(PROGRAM_NAME_FIELD,
                        PROJECT_ID_FIELD,
                        field("IR_AST", String.class))
                .values(programName, projectID, gson.toJson(tree))
                .returningResult(ID_FIELD)
                .fetchOne()
                .into(Long.class);
    }

    public long insertIntermediateCFG(Map<String, Set<?>> irCFGForDB, String programName, long projectID, DSLContext using) {
        return using.insertInto(IR_CFG)
                .columns(PROGRAM_NAME_FIELD,
                        PROJECT_ID_FIELD,
                        field("IR_CFG", String.class))
                .values(programName, projectID, this.gson.toJson(irCFGForDB))
                .returningResult(ID_FIELD)
                .fetchOne()
                .into(Long.class);
    }

    public Long insertLoopBody(NaturalLoopBody<TranspilerInstruction> loopBody, long irCfgID, DSLContext using) {
        return using.insertInto(LOOP_BODY)
                .columns(CFG_ID_FIELD,
                        BODY_FIELD)
                .values(irCfgID, this.gson.toJson(loopBody))
                .returningResult(ID_FIELD)
                .fetchOne()
                .into(Long.class);
    }

    public @NotNull Result<Record2<Long, String>> loopBodiesByCfgID(long cfgID, DSLContext using) {
        return using.select(ID_FIELD, BODY_FIELD)
                .from(LOOP_BODY)
                .where(CFG_ID_FIELD.eq(cfgID))
                .fetch();
    }

    public List<Long> insertLoopBody(Set<NaturalLoopBody<TranspilerInstruction>> loopBodies, long irCfgID, DSLContext using) {
        return loopBodies.stream().map(rlb -> insertLoopBody(rlb, irCfgID, using)).toList();
    }

    public Long insertT1T2AnalysisResult(FlowgraphReductionResult<TranspilerInstruction, DefaultEdge> reductionResult, long irCfgID, DSLContext using) {
        Graph<TranspilerInstruction, DefaultEdge> limitFlowgraph = reductionResult.limitFlowGraph();
        Map<String, Set<?>> limitFlowGraphForDB = ImmutableMap.of("nodes", limitFlowgraph.vertexSet(), "edges", limitFlowgraph.edgeSet());
        return using.insertInto(T1_T2_ANALYSIS)
                .columns(CFG_ID_FIELD,
                        field("IS_REDUCIBLE", Boolean.class),
                        field("LIMIT_FLOW_GRAPH", String.class
                        ))
                .values(irCfgID, reductionResult.isReducible(), this.gson.toJson(limitFlowGraphForDB))
                .returningResult(ID_FIELD)
                .fetchOne()
                .into(Long.class);
    }

    public Optional<Boolean> getT1T2AnalysisResult(long irCfgID, DSLContext using) {
        @Nullable Record3<Long, Boolean, String> t1t2Result = using.select(
                        ID_FIELD,
                        field("IS_REDUCIBLE", Boolean.class),
                        field("LIMIT_FLOW_GRAPH", String.class))
                .from(T1_T2_ANALYSIS)
                .where(CFG_ID_FIELD.eq(irCfgID))
                .fetchOne();
        return t1t2Result != null ? Optional.of(t1t2Result.component2()) : Optional.empty();
    }
}
