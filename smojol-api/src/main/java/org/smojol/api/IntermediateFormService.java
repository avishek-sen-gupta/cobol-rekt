package org.smojol.api;

import com.google.common.collect.ImmutableMap;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import org.jetbrains.annotations.NotNull;
import org.jooq.Record;
import org.jooq.*;
import org.smojol.api.contract.IntermediateASTListing;
import org.smojol.api.contract.IntermediateCFGListing;
import org.smojol.api.contract.ProjectListing;
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
    private final Gson gson;
    Table<Record> IR_AST = table("IR_AST");
    Table<Record> IR_CFG = table("IR_CFG");
    Table<Record> PROJECT = table("PROJECT");

    public IntermediateFormService(Gson gson) {
        this.gson = gson;
    }

    Map<String, List<Map<String, String>>> intermediateASTListingsByProject2(DSLContext using) {
        Result<Record4<String, String, Integer, Integer>> allIntermediateASTs = using
                .select(field("IR_AST.PROGRAM_NAME", String.class),
                        field("IR_AST.IR_AST", String.class),
                        field("IR_AST.ID", Integer.class),
                        field("PROJECT.ID", Integer.class).as("PROJECT_ID"))
                .from(IR_AST)
                .leftOuterJoin(PROJECT)
                .on(field("IR_AST.PROJECT_ID", Integer.class)
                        .eq(field("PROJECT.ID", Integer.class)))
                .fetch();
        Map<String, List<Map<String, String>>> collectedIntermediateASTs = allIntermediateASTs
                .map(ast -> (Map<String, String>) ImmutableMap.of("programName", ast.component1(), "astID", ast.component3().toString(), "projectID", ast.component4().toString())).stream()
                .collect(Collectors.groupingBy(d -> d.get("projectID"))).entrySet().stream()
                .collect(Collectors.toMap(Entry::getKey, Entry::getValue));
        return collectedIntermediateASTs;
    }

    Map<String, List<Map<String, String>>> intermediateCFGListingsByProject(DSLContext using) {
        Result<Record4<String, String, Integer, Integer>> allIntermediateASTs = using
                .select(field("IR_CFG.PROGRAM_NAME", String.class),
                        field("IR_CFG.IR_CFG", String.class),
                        field("IR_CFG.ID", Integer.class),
                        field("PROJECT.ID", Integer.class).as("PROJECT_ID"))
                .from(IR_CFG)
                .leftOuterJoin(PROJECT)
                .on(field("IR_CFG.PROJECT_ID", Integer.class)
                        .eq(field("PROJECT.ID", Integer.class)))
                .fetch();
        Map<String, List<Map<String, String>>> collectedIntermediateCFGs = allIntermediateASTs
                .map(cfg -> (Map<String, String>) ImmutableMap.of("programName", cfg.component1(), "cfgID", cfg.component3().toString(), "projectID", cfg.component4().toString())).stream()
                .collect(Collectors.groupingBy(d -> d.get("projectID"))).entrySet().stream()
                .collect(Collectors.toMap(Entry::getKey, Entry::getValue));
        return collectedIntermediateCFGs;
    }

    List<ProjectListing> allProjectEntities(DSLContext using) {
        Map<String, List<Map<String, String>>> astGroupAsMap = intermediateASTListingsByProject2(using);
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

    public Optional<Map<String, Object>> intermediateAST(int id, DSLContext using) {
        @NotNull Result<Record2<Integer, String>> ast = using.select(field("id", Integer.class), field("IR_AST", String.class)).from(IR_AST).where(field("ID", Integer.class).eq(id)).fetch();
        if (ast.isEmpty()) return Optional.empty();
        return Optional.of(ImmutableMap.of("id", id, "ast", gson.fromJson(ast.getFirst().component2(), JsonObject.class)));
    }

    public Optional<Map<String, Object>> intermediateCFG(int id, DSLContext using) {
        @NotNull Result<Record2<Integer, String>> ast = using.select(field("id", Integer.class), field("IR_CFG", String.class))
                .from(IR_CFG)
                .where(field("ID", Integer.class).eq(id))
                .fetch();
        if (ast.isEmpty()) return Optional.empty();
        return Optional.of(ImmutableMap.of("id", id, "cfg", gson.fromJson(ast.getFirst().component2(), JsonObject.class)));
    }

    public long insertIntermediateAST(TranspilerNode tree, String programName, long projectID, DSLContext using) {
        return using.insertInto(table("IR_AST"))
                .columns(field("PROGRAM_NAME"), field("PROJECT_ID"),
                        field("IR_AST"))
                .values(programName, projectID, gson.toJson(tree))
                .returningResult(field("ID", Long.class))
                .fetchOne()
                .into(Long.class);
    }

    public long insertIntermediateCFG(Map<String, Set<?>> irCFGForDB, String programName, long projectID, DSLContext using) {
        return using.insertInto(table("IR_CFG"))
                .columns(field("PROGRAM_NAME"), field("PROJECT_ID"),
                        field("IR_CFG"))
                .values(programName, projectID, this.gson.toJson(irCFGForDB))
                .returningResult(field("ID", Long.class))
                .fetchOne()
                .into(Long.class);
    }
}
