package org.smojol.api;

import com.google.common.collect.ImmutableMap;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import org.jetbrains.annotations.NotNull;
import org.jooq.DSLContext;
import org.jooq.Record2;
import org.jooq.Result;
import org.smojol.toolkit.analysis.graph.graphml.SerialisableUnifiedModel;

import java.util.Map;
import java.util.Optional;

import static org.jooq.impl.DSL.field;
import static org.smojol.api.pipeline.DbConstants.*;

public class SourceService {
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
}
