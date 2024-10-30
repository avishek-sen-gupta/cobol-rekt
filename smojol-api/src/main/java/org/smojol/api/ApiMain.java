package org.smojol.api;

import io.javalin.Javalin;

public class ApiMain {
    public static void main(String[] args) {
        Javalin.create()
                .get("/api/heartbeat", ctx -> ctx.result("Hello World!"))
                .start(7070);
    }
}
