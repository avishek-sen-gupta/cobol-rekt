package org.smojol.api.database;

import org.jooq.DSLContext;
import org.jooq.SQLDialect;
import org.jooq.impl.DSL;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.function.Function;

public class DbContext {
    public static final String DATABASE_URL = "DATABASE_URL";
    public static final String DATABASE_USER = "DATABASE_USER";
    public static final String DATABASE_PASSWORD = "DATABASE_PASSWORD";
    private final String url;
    private final String user;
    private final String password;

    public DbContext(String url, String user, String password) {
        this.url = url;
        this.user = user;
        this.password = password;
    }

    public <R> R execute(Function<DSLContext, R> dbBlock) throws SQLException {
        try (Connection conn = DriverManager.getConnection(url, user, password)) {
            DSLContext using = DSL.using(conn, SQLDialect.SQLITE);
            return dbBlock.apply(using);
        }
    }

    public static DbContext fromSystemEnv() {
        return new DbContext(
                System.getenv(DATABASE_URL),
                System.getenv(DATABASE_USER),
                System.getenv(DATABASE_PASSWORD));
    }
}
