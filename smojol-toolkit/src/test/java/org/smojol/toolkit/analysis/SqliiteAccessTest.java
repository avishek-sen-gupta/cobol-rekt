package org.smojol.toolkit.analysis;

import io.github.cdimascio.dotenv.Dotenv;
import org.jooq.*;
import org.jooq.Record;
import org.jooq.impl.DSL;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

import static org.jooq.impl.DSL.*;

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
            Result<Record> records = using.select().from("PROJECT").fetch();
            System.out.println(records);
        }
    }
}
