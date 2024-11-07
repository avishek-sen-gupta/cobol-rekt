package org.smojol.api;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.jooq.*;
import org.smojol.api.contract.*;
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
import static org.smojol.api.SourceService.FLOWCHART;
import static org.smojol.api.pipeline.DbConstants.*;

public class IntermediateFormService {
    private final Gson gson;

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

    Map<String, List<Map<String, String>>> unifiedFlowListingsByProject(DSLContext using) {
        @NotNull Result<Record3<String, Long, Long>> allFlowModels = using
                .select(field("UNIFIED_FLOW.PROGRAM_NAME", String.class),
                        field("UNIFIED_FLOW.ID", Long.class),
                        field("PROJECT.ID", Long.class).as(PROJECT_ID))
                .from(UNIFIED_FLOW)
                .leftOuterJoin(PROJECT)
                .on(field("UNIFIED_FLOW.PROJECT_ID", Long.class)
                        .eq(field("PROJECT.ID", Long.class)))
                .fetch();
        Map<String, List<Map<String, String>>> collectedFlowModels = allFlowModels
                .map(cfg -> (Map<String, String>) ImmutableMap.of("programName", cfg.component1(), "flowModelID", cfg.component2().toString(), "projectID", cfg.component3().toString())).stream()
                .collect(Collectors.groupingBy(d -> d.get("projectID"))).entrySet().stream()
                .collect(Collectors.toMap(Entry::getKey, Entry::getValue));
        return collectedFlowModels;
    }

    Map<String, List<Map<String, String>>> flowchartListingsByProject(DSLContext using) {
        @NotNull Result<Record3<String, Long, Long>> allFlowModels = using
                .select(field("FLOWCHART.PROGRAM_NAME", String.class),
                        field("FLOWCHART.ID", Long.class),
                        field("PROJECT.ID", Long.class).as(PROJECT_ID))
                .from(FLOWCHART)
                .leftOuterJoin(PROJECT)
                .on(field("FLOWCHART.PROJECT_ID", Long.class)
                        .eq(field("PROJECT.ID", Long.class)))
                .fetch();
        Map<String, List<Map<String, String>>> collectedFlowcharts = allFlowModels
                .map(cfg -> (Map<String, String>) ImmutableMap.of("programName", cfg.component1(), "flowchartID", cfg.component2().toString(), "projectID", cfg.component3().toString())).stream()
                .collect(Collectors.groupingBy(d -> d.get("projectID"))).entrySet().stream()
                .collect(Collectors.toMap(Entry::getKey, Entry::getValue));
        return collectedFlowcharts;
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
        Map<String, List<Map<String, String>>> unifiedFlowGroupAsMap = unifiedFlowListingsByProject(using);
        Map<String, List<Map<String, String>>> flowchartGroupAsMap = flowchartListingsByProject(using);
        Set<Entry<String, List<Map<String, String>>>> cfgsGroupedByProjectID = cfgGroupAsMap.entrySet();
        Set<Entry<String, List<Map<String, String>>>> astsGroupedByProjectID = astGroupAsMap.entrySet();
        Set<Entry<String, List<Map<String, String>>>> unifiedFlowGroupedByProjectID = unifiedFlowGroupAsMap.entrySet();
        Set<Entry<String, List<Map<String, String>>>> flowchartsGroupedByProjectID = flowchartGroupAsMap.entrySet();
        Stream<String> cfgUniqueProjectIDs = cfgsGroupedByProjectID.stream().map(Entry::getKey);
        Stream<String> astUniqueProjectIDs = astsGroupedByProjectID.stream().map(Entry::getKey);
        Stream<String> unifiedFlowUniqueProjectIDs = unifiedFlowGroupedByProjectID.stream().map(Entry::getKey);
        Stream<String> flowchartUniqueProjectIDs = flowchartsGroupedByProjectID.stream().map(Entry::getKey);
        Set<String> allUniqueProjectIDs = Stream.of(cfgUniqueProjectIDs, astUniqueProjectIDs, unifiedFlowUniqueProjectIDs, flowchartUniqueProjectIDs).flatMap(s -> s).collect(Collectors.toUnmodifiableSet());

        List<ProjectListing> projectListings = allUniqueProjectIDs.stream().map(pid -> new ProjectListing(pid,
                irASTs(pid, astGroupAsMap),
                irCFGs(pid, cfgGroupAsMap),
                flowModels(pid, unifiedFlowGroupAsMap),
                flowcharts(pid, flowchartGroupAsMap)
        )).toList();

        return projectListings;
    }

    private static @NotNull List<IntermediateCFGListing> irCFGs(String pid, Map<String, List<Map<String, String>>> cfgGroupAsMap) {
        List<Map<String, String>> irCFGsForProject = cfgGroupAsMap.get(pid);
        if (irCFGsForProject == null) return ImmutableList.of();
        return irCFGsForProject.stream()
                .map(cfg -> new IntermediateCFGListing(cfg.get("cfgID"), cfg.get("programName")))
                .toList();
    }

    private static @NotNull List<IntermediateASTListing> irASTs(String pid, Map<String, List<Map<String, String>>> astGroupAsMap) {
        List<Map<String, String>> irASTsForProject = astGroupAsMap.get(pid);
        if (irASTsForProject == null) return ImmutableList.of();
        return irASTsForProject.stream()
                .map(ast -> new IntermediateASTListing(ast.get("astID"), ast.get("programName")))
                .toList();
    }

    private static @NotNull List<UnifiedFlowModelListing> flowModels(String pid, Map<String, List<Map<String, String>>> unifiedFlowGroupAsMap) {
        List<Map<String, String>> modelsForProject = unifiedFlowGroupAsMap.get(pid);
        if (modelsForProject == null) return ImmutableList.of();
        return modelsForProject.stream()
                .map(cfg -> new UnifiedFlowModelListing(cfg.get("flowModelID"), cfg.get("programName")))
                .toList();
    }

    private static @NotNull List<FlowchartListing> flowcharts(String pid, Map<String, List<Map<String, String>>> unifiedFlowGroupAsMap) {
        List<Map<String, String>> modelsForProject = unifiedFlowGroupAsMap.get(pid);
        if (modelsForProject == null) return ImmutableList.of();
        return modelsForProject.stream()
                .map(cfg -> new FlowchartListing(cfg.get("flowchartID"), cfg.get("programName")))
                .toList();
    }

    public Optional<Map<String, Object>> intermediateAST(long id, DSLContext using) {
        @NotNull Result<Record2<Long, String>> ast = using.select(ID_FIELD, field("IR_AST", String.class))
                .from(IR_AST)
                .where(ID_FIELD.eq(id))
                .fetch();
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
