package org.smojol.api;

import org.jooq.DSLContext;
import org.jooq.Record;
import org.jooq.Table;
import org.smojol.api.database.ConnectionBuilder;

import static org.jooq.impl.DSL.field;
import static org.jooq.impl.DSL.table;

public class ProjectService {
    private final ConnectionBuilder connectionBuilder;
    Table<Record> PROJECT = table("PROJECT");

    public ProjectService(ConnectionBuilder connectionBuilder) {
        this.connectionBuilder = connectionBuilder;
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
