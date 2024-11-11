package org.smojol.api;

import com.google.common.collect.ImmutableMap;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import org.jetbrains.annotations.NotNull;
import org.jooq.Record;
import org.jooq.*;
import org.smojol.common.ast.CobolContextAugmentedTreeNode;
import org.smojol.toolkit.analysis.graph.graphml.SerialisableUnifiedModel;

import java.util.Map;
import java.util.Optional;

import static org.jooq.impl.DSL.field;
import static org.jooq.impl.DSL.table;
import static org.smojol.api.pipeline.DbConstants.*;

public class SourceService {
    public static final @NotNull Table<Record> FLOWCHART = table("FLOWCHART");
    public static final @NotNull Field<String> MARKUP_FIELD = field("MARKUP", String.class);
    public static final @NotNull Table<Record> RAW_AST = table("RAW_AST");
    private final Gson gson;

    public SourceService(Gson gson) {
        this.gson = gson;
    }

    public Long insertUnifiedModel(SerialisableUnifiedModel model, String programName, long projectID, DSLContext using) {
        return using.insertInto(UNIFIED_FLOW)
                .columns(PROGRAM_NAME_FIELD,
                        PROJECT_ID_FIELD,
                        BODY_FIELD)
                .values(programName, projectID, gson.toJson(model))
                .returningResult(ID_FIELD)
                .fetchOne()
                .into(Long.class);
    }

    public Optional<Map<String, Object>> flowModel(long id, DSLContext using) {
        @NotNull Result<Record2<Long, String>> flowModels = using.select(ID_FIELD, BODY_FIELD)
                .from(UNIFIED_FLOW)
                .where(ID_FIELD.eq(id))
                .fetch();
        if (flowModels.isEmpty()) return Optional.empty();
        return Optional.of(ImmutableMap.of("id", id, "body", gson.fromJson(flowModels.getFirst().component2(), JsonObject.class)));
    }

    public Optional<Map<String, Object>> rawAST(long id, DSLContext using) {
        @NotNull Result<Record2<Long, String>> rawASTs = using.select(ID_FIELD, AST_BODY_FIELD)
                .from(RAW_AST)
                .where(ID_FIELD.eq(id))
                .fetch();
        if (rawASTs.isEmpty()) return Optional.empty();
        return Optional.of(ImmutableMap.of("id", id, "ast", gson.fromJson(rawASTs.getFirst().component2(), JsonObject.class)));
    }

    public Long insertFlowchart(String flowchartMarkup, String programName, long projectID, String sectionName, DSLContext using) {
        return using.insertInto(FLOWCHART)
                .columns(PROGRAM_NAME_FIELD,
                        PROJECT_ID_FIELD,
                        field("SECTION"),
                        MARKUP_FIELD)
                .values(programName, projectID, sectionName, flowchartMarkup)
                .returningResult(ID_FIELD)
                .fetchOne()
                .into(Long.class);
    }

    public Optional<Map<String, Object>> flowchart(long id, DSLContext using) {
        @NotNull Result<Record2<Long, String>> flowcharts = using.select(ID_FIELD, MARKUP_FIELD)
                .from(FLOWCHART)
                .where(ID_FIELD.eq(id))
                .fetch();
        if (flowcharts.isEmpty()) return Optional.empty();
        return Optional.of(ImmutableMap.of("id", id, "markup", flowcharts.getFirst().component2()));
    }

    public long insertRawAST(CobolContextAugmentedTreeNode serialisableAST, String programName, long projectID, DSLContext using) {
        Gson gson = new GsonBuilder().excludeFieldsWithoutExposeAnnotation().setPrettyPrinting().create();

        return using.insertInto(RAW_AST)
                .columns(PROGRAM_NAME_FIELD,
                        PROJECT_ID_FIELD,
                        AST_BODY_FIELD)
                .values(programName, projectID, gson.toJson(serialisableAST))
                .returningResult(ID_FIELD)
                .fetchOne()
                .into(Long.class);
    }
}
