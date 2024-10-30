package org.smojol.api;

import io.javalin.Javalin;

import java.util.logging.Logger;

public class ApiMain {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(ApiMain.class.getName());
    public static void main(String[] args) {
        Javalin.create(config -> {
                    config.staticFiles.add("/static/dist");
                    config.requestLogger.http((ctx, ms) -> LOGGER.info("Got a request: " + ctx.path()));
                })
                .get("/api/heartbeat", ctx -> ctx.result("Hello World!"))
                .start(7070);
    }
}
