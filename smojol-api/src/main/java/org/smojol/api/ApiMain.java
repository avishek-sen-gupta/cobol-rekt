package org.smojol.api;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import io.javalin.json.JsonMapper;
import org.jetbrains.annotations.NotNull;
import org.smojol.api.database.DbContext;

import java.io.IOException;
import java.lang.reflect.Type;
import java.net.URISyntaxException;
import java.sql.SQLException;

import static org.smojol.api.database.DbContext.fromSystemEnv;

public class ApiMain {
    public static void main(String[] args) throws IOException, URISyntaxException, SQLException {
        Gson gson = new GsonBuilder().create();
        JsonMapper gsonMapper = getJsonMapper(gson);
        DbContext dbContext = fromSystemEnv();

        new ApiServer().runServer(7070, gsonMapper, gson, dbContext);
    }

    private static @NotNull JsonMapper getJsonMapper(Gson gson) {
        return new JsonMapper() {
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
    }
}
