package org.smojol.api;

import com.google.gson.Gson;
import org.jooq.DSLContext;
import org.smojol.toolkit.analysis.graph.graphml.SerialisableUnifiedModel;

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
}
