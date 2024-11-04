package org.smojol.api;

import org.jooq.DSLContext;
import org.jooq.Record;
import org.jooq.Table;

import static org.jooq.impl.DSL.field;
import static org.jooq.impl.DSL.table;

public class ProjectService {
    Table<Record> PROJECT = table("PROJECT");

    public ProjectService() {
    }

    public long insertProject(String projectName, DSLContext using) {
        Long into = using.insertInto(PROJECT)
                .columns(field("NAME"))
                .values(projectName)
                .returningResult(field("ID", Long.class))
                .fetchOne()
                .into(Long.class);
        return into;
    }
}
