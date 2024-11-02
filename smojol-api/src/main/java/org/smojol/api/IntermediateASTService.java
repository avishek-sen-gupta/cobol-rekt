package org.smojol.api;

import com.google.common.collect.ImmutableMap;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import org.jetbrains.annotations.NotNull;
import org.jooq.Record;
import org.jooq.*;
import org.smojol.api.contract.IntermediateASTListing;
import org.smojol.api.contract.ProjectListing;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import static org.jooq.impl.DSL.field;
import static org.jooq.impl.DSL.table;

public class IntermediateASTService {
    Table<Record> IR_AST = table("IR_AST");
    Table<Record> PROJECT = table("PROJECT");

    List<ProjectListing> intermediateASTListingsByProject(DSLContext using) {
        Result<Record4<String, String, Integer, Integer>> allIntermediateASTs = using
                .select(field("IR_AST.PROGRAM_NAME", String.class),
                        field("IR_AST.IR_AST", String.class),
                        field("IR_AST.ID", Integer.class),
                        field("PROJECT.ID", Integer.class).as("PROJECT_ID"))
                .from(IR_AST)
                .join(PROJECT)
                .on(field("IR_AST.PROJECT_ID", Integer.class)
                        .eq(field("PROJECT.ID", Integer.class)))
                .fetch();
        List<ProjectListing> collectedIntermediateASTs = allIntermediateASTs
                .map(ast -> (Map<String, String>) ImmutableMap.of("programName", ast.component1(), "astID", ast.component3().toString(), "projectID", ast.component4().toString())).stream()
                .collect(Collectors.groupingBy(d -> d.get("projectID"))).entrySet().stream()
//                .map(entry -> (Map<String, Object>) ImmutableMap.of("projectID", entry.getKey(), "asts", entry.getValue())).toList();
                .map(entry -> new ProjectListing(entry.getKey(), entry.getValue().stream()
                        .map(p -> new IntermediateASTListing(p.get("astID"), p.get("programName")))
                        .toList()))
                .toList();
        return collectedIntermediateASTs;
    }

    public Optional<Map<String, Object>> intermediateAST(int id, DSLContext using) {
        Gson gson = new Gson();
        @NotNull Result<Record2<Integer, String>> ast = using.select(field("id", Integer.class), field("IR_AST", String.class)).from(IR_AST).where(field("ID", Integer.class).eq(id)).fetch();
        if (ast.isEmpty()) return Optional.empty();
        return Optional.of(ImmutableMap.of("id", id, "ast", gson.fromJson(ast.getFirst().component2(), JsonObject.class)));
    }
}
