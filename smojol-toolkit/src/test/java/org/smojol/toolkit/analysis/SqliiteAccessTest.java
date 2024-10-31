package org.smojol.toolkit.analysis;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import org.jooq.DSLContext;
import org.jooq.Record;
import org.jooq.Result;
import org.jooq.SQLDialect;
import org.jooq.impl.DSL;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

import static org.jooq.impl.DSL.field;
import static org.jooq.impl.DSL.table;

public class SqliiteAccessTest {
    @Test
    @Disabled
    public void canAccessDB() throws SQLException {
        String url = System.getenv("DATABASE_URL");
        String user = System.getenv("DATABASE_USER");
        String password = System.getenv("DATABASE_PASSWORD");
        try (Connection conn = DriverManager.getConnection(url, user, password)) {
            DSLContext using = DSL.using(conn, SQLDialect.MYSQL);
            int result = using.insertInto(table("PROJECT"))
                    .columns(field("NAME"))
                    .values("PROJECT-1").execute();
            JumpIfTranspilerNode transpilerNode = new JumpIfTranspilerNode(new NamedLocationNode("ABCD"), new EqualToNode(new ValueOfNode(new SymbolReferenceNode("A")), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(10))));
            String json = new Gson().toJson(transpilerNode);
            JsonObject jsonObject = new Gson().fromJson(json, JsonObject.class);
            using.insertInto(table("RAW_AST"))
                    .columns(field("PROGRAM_NAME"), field("PROJECT_ID"), field("AST"))
                    .values("some_program.cbl", 1, json).execute();
            Result<Record> records = using.select().from("RAW_AST").fetch();
            System.out.println(records);
        }
    }
}
