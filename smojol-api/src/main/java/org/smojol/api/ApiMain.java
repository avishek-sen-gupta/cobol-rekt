package org.smojol.api;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import io.javalin.Javalin;
import io.javalin.http.Handler;
import io.javalin.json.JsonMapper;
import org.apache.commons.io.IOUtils;
import org.jetbrains.annotations.NotNull;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Type;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.logging.Logger;

public class ApiMain {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(ApiMain.class.getName());

    public static void main(String[] args) throws IOException, URISyntaxException {
        ClassLoader classloader = Thread.currentThread().getContextClassLoader();
        InputStream is = classloader.getResourceAsStream("test-data/data.json");
        String content = IOUtils.toString(is, StandardCharsets.UTF_8);
        Handler dummyAST = ctx -> {
            JumpIfTranspilerNode transpilerNode = new JumpIfTranspilerNode(new NamedLocationNode("ABCD"), new EqualToNode(new ValueOfNode(new SymbolReferenceNode("A")), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(10))));
//            JsonObject n = new Gson().fromJson("""
//                    {"name":"ProcedureDivisionBodyContext/T1","nodeType":"LabelledTranspilerCodeBlockNode","properties":{"type":"PROCEDURE_DIVISION_BODY"},"categories":["CODE_BLOCK"],"childTranspilerNodes":[{"nodeType":"TranspilerCodeBlockNode","properties":{},"categories":["CODE_BLOCK"],"childTranspilerNodes":[],"id":"a7e336ef-7fa6-45a7-807d-b7cbf086b037"},{"name":"S","nodeType":"LabelledTranspilerCodeBlockNode","properties":{"type":"SECTION"},"categories":["CODE_BLOCK"],"childTranspilerNodes":[{"s":"S SECTION","nodeType":"PlaceholderTranspilerNode","properties":{},"categories":["UNKNOWN"],"childTranspilerNodes":[],"id":"3e86a6c7-2a0c-4dda-88e4-a0787f648cb0"},{"nodeType":"TranspilerCodeBlockNode","properties":{},"categories":["CODE_BLOCK"],"childTranspilerNodes":[{"name":"SA1","nodeType":"LabelledTranspilerCodeBlockNode","properties":{"type":"PARAGRAPH"},"categories":["CODE_BLOCK"],"childTranspilerNodes":[{"s":"SA1","nodeType":"PlaceholderTranspilerNode","properties":{},"categories":["UNKNOWN"],"childTranspilerNodes":[],"id":"f7159358-d9f2-4db1-afef-3a2313375504"},{"nodeType":"TranspilerCodeBlockNode","properties":{"type":"SENTENCE"},"categories":["CODE_BLOCK"],"childTranspilerNodes":[{"condition":{"lhs":{"name":"WS-NUM1","nodeType":"SymbolReferenceNode","properties":{},"categories":["REFERENCE"],"childTranspilerNodes":[],"id":"bf864385-a649-4e13-89f0-afcc2e808761"},"rhs":{"value":{"value":10.0,"dataType":{"usageType":"ZONED_DECIMAL","defaultValue":{"value":"NULL","valueBasedComparator":{}},"abstractType":"NUMBER"},"valueBasedComparator":{}},"nodeType":"PrimitiveValueTranspilerNode","properties":{},"categories":["REFERENCE"],"childTranspilerNodes":[],"id":"4f9762a2-f60d-496f-b58d-050fc5bd342c"},"nodeType":"GreaterThanNode","properties":{},"categories":["COMPARISON"],"childTranspilerNodes":[],"id":"dd8b8329-ef06-4861-abfa-300824781925"},"ifThenBlock":{"nodeType":"DetachedTranspilerCodeBlockNode","properties":{},"categories":["CODE_BLOCK"],"childTranspilerNodes":[{"operands":[{"expression":{"value":{"value":"\\"\\u003e 10\\"","dataType":{"usageType":"DEFAULT","defaultValue":{"value":"","valueBasedComparator":{}},"abstractType":"STRING"},"valueBasedComparator":{}},"nodeType":"PrimitiveValueTranspilerNode","properties":{},"categories":["REFERENCE"],"childTranspilerNodes":[],"id":"5d9b37e9-814b-4143-a42a-962738ccfe44"},"nodeType":"ValueOfNode","properties":{},"categories":["DEREFERENCE"],"childTranspilerNodes":[],"id":"f4487f5a-8fca-494a-bc27-eba0335d134c"}],"nodeType":"PrintTranspilerNode","properties":{},"categories":["IO"],"childTranspilerNodes":[],"id":"b4095afb-b36b-4d79-a44b-f605bd70c7a2"},{"start":{"nodeType":"NextLocationNode","properties":{},"categories":["ADDRESS"],"childTranspilerNodes":[],"id":"1295d28f-1d7d-4d7d-b6c9-dc8d6a708ddd"},"nodeType":"JumpTranspilerNode","properties":{},"categories":["CONTROL_FLOW"],"childTranspilerNodes":[],"id":"0c04cd56-b7e5-479c-8419-986f05d090fb"}],"id":"a724eb68-5373-4cc0-8be4-66594922f249"},"ifElseBlock":{"nodeType":"DetachedTranspilerCodeBlockNode","properties":{},"categories":["CODE_BLOCK"],"childTranspilerNodes":[{"operands":[{"expression":{"value":{"value":"\\"\\u003c\\u003d 10\\"","dataType":{"usageType":"DEFAULT","defaultValue":{"value":"","valueBasedComparator":{}},"abstractType":"STRING"},"valueBasedComparator":{}},"nodeType":"PrimitiveValueTranspilerNode","properties":{},"categories":["REFERENCE"],"childTranspilerNodes":[],"id":"ce0be6a7-b95b-4ecf-86ff-c392e922e3c7"},"nodeType":"ValueOfNode","properties":{},"categories":["DEREFERENCE"],"childTranspilerNodes":[],"id":"fc29f235-c71c-4b25-bdfa-0c6354efc5b6"}],"nodeType":"PrintTranspilerNode","properties":{},"categories":["IO"],"childTranspilerNodes":[],"id":"44b5548a-4ff3-43c6-bf31-5a80ebc31714"}],"id":"9e8250c9-cc97-4198-b025-1ed44babcc63"},"nodeType":"IfTranspilerNode","properties":{},"categories":["DECISION"],"childTranspilerNodes":[{"nodeType":"DetachedTranspilerCodeBlockNode","properties":{},"categories":["CODE_BLOCK"],"childTranspilerNodes":[{"operands":[{"expression":{"value":{"value":"\\"\\u003e 10\\"","dataType":{"usageType":"DEFAULT","defaultValue":{"value":"","valueBasedComparator":{}},"abstractType":"STRING"},"valueBasedComparator":{}},"nodeType":"PrimitiveValueTranspilerNode","properties":{},"categories":["REFERENCE"],"childTranspilerNodes":[],"id":"5d9b37e9-814b-4143-a42a-962738ccfe44"},"nodeType":"ValueOfNode","properties":{},"categories":["DEREFERENCE"],"childTranspilerNodes":[],"id":"f4487f5a-8fca-494a-bc27-eba0335d134c"}],"nodeType":"PrintTranspilerNode","properties":{},"categories":["IO"],"childTranspilerNodes":[],"id":"b4095afb-b36b-4d79-a44b-f605bd70c7a2"},{"start":{"nodeType":"NextLocationNode","properties":{},"categories":["ADDRESS"],"childTranspilerNodes":[],"id":"1295d28f-1d7d-4d7d-b6c9-dc8d6a708ddd"},"nodeType":"JumpTranspilerNode","properties":{},"categories":["CONTROL_FLOW"],"childTranspilerNodes":[],"id":"0c04cd56-b7e5-479c-8419-986f05d090fb"}],"id":"a724eb68-5373-4cc0-8be4-66594922f249"},{"nodeType":"DetachedTranspilerCodeBlockNode","properties":{},"categories":["CODE_BLOCK"],"childTranspilerNodes":[{"operands":[{"expression":{"value":{"value":"\\"\\u003c\\u003d 10\\"","dataType":{"usageType":"DEFAULT","defaultValue":{"value":"","valueBasedComparator":{}},"abstractType":"STRING"},"valueBasedComparator":{}},"nodeType":"PrimitiveValueTranspilerNode","properties":{},"categories":["REFERENCE"],"childTranspilerNodes":[],"id":"ce0be6a7-b95b-4ecf-86ff-c392e922e3c7"},"nodeType":"ValueOfNode","properties":{},"categories":["DEREFERENCE"],"childTranspilerNodes":[],"id":"fc29f235-c71c-4b25-bdfa-0c6354efc5b6"}],"nodeType":"PrintTranspilerNode","properties":{},"categories":["IO"],"childTranspilerNodes":[],"id":"44b5548a-4ff3-43c6-bf31-5a80ebc31714"}],"id":"9e8250c9-cc97-4198-b025-1ed44babcc63"}],"id":"68a23af9-b060-431a-9e72-f3eb4aba6fbf"}],"id":"65a09921-7831-472d-84a6-dd90608ffc32"},{"nodeType":"TranspilerCodeBlockNode","properties":{"type":"SENTENCE"},"categories":["CODE_BLOCK"],"childTranspilerNodes":[{"operands":[{"expression":{"value":{"value":"\\"SA1\\"","dataType":{"usageType":"DEFAULT","defaultValue":{"value":"","valueBasedComparator":{}},"abstractType":"STRING"},"valueBasedComparator":{}},"nodeType":"PrimitiveValueTranspilerNode","properties":{},"categories":["REFERENCE"],"childTranspilerNodes":[],"id":"ddd2a966-6eff-4b12-acdb-7af8c6c393f9"},"nodeType":"ValueOfNode","properties":{},"categories":["DEREFERENCE"],"childTranspilerNodes":[],"id":"8b8991f2-cbf5-4a42-9c44-8ab0f70b8460"}],"nodeType":"PrintTranspilerNode","properties":{},"categories":["IO"],"childTranspilerNodes":[],"id":"0c25acbb-129e-458a-b69e-4802f814ee67"}],"id":"f9f0a633-ff52-4152-85e2-d6479a60bef5"}],"id":"48a15824-2974-43f1-a77b-87ae36aceaee"},{"name":"SZ1","nodeType":"LabelledTranspilerCodeBlockNode","properties":{"type":"PARAGRAPH"},"categories":["CODE_BLOCK"],"childTranspilerNodes":[{"s":"SZ1","nodeType":"PlaceholderTranspilerNode","properties":{},"categories":["UNKNOWN"],"childTranspilerNodes":[],"id":"54970a6e-426c-4c61-99fb-97eca346180f"},{"nodeType":"TranspilerCodeBlockNode","properties":{"type":"SENTENCE"},"categories":["CODE_BLOCK"],"childTranspilerNodes":[{"operands":[{"expression":{"value":{"value":"\\"ENDING...\\"","dataType":{"usageType":"DEFAULT","defaultValue":{"value":"","valueBasedComparator":{}},"abstractType":"STRING"},"valueBasedComparator":{}},"nodeType":"PrimitiveValueTranspilerNode","properties":{},"categories":["REFERENCE"],"childTranspilerNodes":[],"id":"9f78b327-3dae-42a6-8ada-465c0ecd38ff"},"nodeType":"ValueOfNode","properties":{},"categories":["DEREFERENCE"],"childTranspilerNodes":[],"id":"16c129b4-63be-4a3d-87a2-13546339c5d9"}],"nodeType":"PrintTranspilerNode","properties":{},"categories":["IO"],"childTranspilerNodes":[],"id":"87f238db-994e-4568-babb-2ec5ad97fb14"}],"id":"7621ebfb-8fcb-456e-aadf-86641d072ea0"}],"id":"cc6941f0-a845-41ac-8dbf-d5d873f165a9"}],"id":"078b38dd-a344-489e-a8f7-517fd13a0c8e"}],"id":"6cf1f075-2421-4c1c-8e3b-6a59e2204277"}],"id":"0c4c673f-3b32-4d43-ac82-6bac8728e32f"}
//                    """, JsonObject.class);

            JsonObject n = new Gson().fromJson(content, JsonObject.class);
            ctx.json(n);
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
