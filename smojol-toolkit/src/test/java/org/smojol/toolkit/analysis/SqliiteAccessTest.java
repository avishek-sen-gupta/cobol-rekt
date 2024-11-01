package org.smojol.toolkit.analysis;

import com.google.common.collect.ImmutableMap;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import org.jooq.Record;
import org.jooq.*;
import org.jooq.impl.DSL;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.jooq.impl.DSL.field;
import static org.jooq.impl.DSL.table;

public class SqliiteAccessTest {
    @Test
    @Disabled
    public void canAccessDB() throws SQLException {
        String url = System.getenv("DATABASE_URL");
        String user = System.getenv("DATABASE_USER");
        String password = System.getenv("DATABASE_PASSWORD");
        System.out.println("URL is " + url);
        try (Connection conn = DriverManager.getConnection(url, user, password)) {
            DSLContext using = DSL.using(conn, SQLDialect.SQLITE);
            Table<Record> PROJECT = table("PROJECT");
            int result = using.insertInto(PROJECT)
                    .columns(field("NAME"))
                    .values("PROJECT-1").execute();
            JumpIfTranspilerNode transpilerNode = new JumpIfTranspilerNode(new NamedLocationNode("ABCD"), new EqualToNode(new ValueOfNode(new SymbolReferenceNode("A")), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(10))));
            String json = new Gson().toJson(transpilerNode);
            JsonObject jsonObject = new Gson().fromJson(json, JsonObject.class);
            Table<Record> IR_AST = table("IR_AST");
            Result<Record1<String>> names = using.select(IR_AST.field("PROGRAM_NAME", String.class)).from(IR_AST).fetch();
            System.out.println("NAMESSSSS>>>>>>>>>>>>>>>>>>>>>");
            System.out.println(names);
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

//            List<Map<String, Object>> collectedIntermediateASTs = new Intermed intermediateASTListingsByProject(allIntermediateASTs);
//            String astsByProjectString = new Gson().toJson(collectedIntermediateASTs);
//            System.out.println(astsByProjectString);
//            using.insertInto(table("RAW_AST"))
//                    .columns(field("PROGRAM_NAME"), field("PROJECT_ID"), field("AST"))
//                    .values("some_program.cbl", 1, json).execute();
//            Result<Record> records = using.select().from("RAW_AST").fetch();
//            System.out.println(records);
        }
    }
}
