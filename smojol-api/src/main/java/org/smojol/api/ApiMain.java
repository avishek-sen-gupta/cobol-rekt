package org.smojol.api;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import io.javalin.Javalin;
import io.javalin.http.Handler;
import io.javalin.json.JsonMapper;
import org.jetbrains.annotations.NotNull;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;

import java.lang.reflect.Type;
import java.util.logging.Logger;

public class ApiMain {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(ApiMain.class.getName());

    public static void main(String[] args) {
        Handler dummyAST = ctx -> {
            JumpIfTranspilerNode transpilerNode = new JumpIfTranspilerNode(new NamedLocationNode("ABCD"), new EqualToNode(new ValueOfNode(new SymbolReferenceNode("A")), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(10))));
            ctx.json(transpilerNode);
        };

        Gson gson = new GsonBuilder().create();
        JsonMapper gsonMapper = new JsonMapper() {
            @NotNull
            @Override
            public String toJsonString(@NotNull Object obj, @NotNull Type type) {
                return gson.toJson(obj, type);
            }

            @NotNull
            @Override
            public <T> T fromJsonString(@NotNull String json, @NotNull Type targetType) {
                return gson.fromJson(json, targetType);
            }
        };
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.setVisibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY);
        Javalin.create(config -> {
                    config.jsonMapper(gsonMapper);
                    config.staticFiles.add("/static/dist");
                    config.requestLogger.http((ctx, ms) -> LOGGER.info("Got a request: " + ctx.path()));
                })
                .get("/api/heartbeat", ctx -> ctx.result("Hello World!"))
                .get("/api/ir-ast", dummyAST)
                .start(7070);
    }
}
