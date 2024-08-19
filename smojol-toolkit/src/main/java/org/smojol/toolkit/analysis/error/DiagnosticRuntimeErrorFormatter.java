package org.smojol.toolkit.analysis.error;

import com.google.gson.*;
import lombok.Getter;
import org.antlr.v4.runtime.RecognitionException;
import org.eclipse.lsp.cobol.common.error.SyntaxError;

import java.util.List;
import java.util.Optional;

@Getter
public class DiagnosticRuntimeErrorFormatter {

    private final Gson gson;

    public DiagnosticRuntimeErrorFormatter() {
        gson = new GsonBuilder().setPrettyPrinting().addSerializationExclusionStrategy(new ExclusionStrategy() {
            @Override
            public boolean shouldSkipField(FieldAttributes fieldAttributes) {
                return "recognizer".equals(fieldAttributes.getName()) || "children".equals(fieldAttributes.getName())
                        || "exception".equals(fieldAttributes.getName())
                        || "ctx".equals(fieldAttributes.getName())
                        || "contextUsages".equals(fieldAttributes.getName());
            }

            @Override
            public boolean shouldSkipClass(Class<?> aClass) {
                return RecognitionException.class.equals(aClass);
            }
        }).create();
    }

    public String formatted(List<SyntaxError> errors) {
        JsonArray diagnostics = new JsonArray();
        List<JsonObject> diagnoses = errors.stream().map(this::toJson).toList();
        diagnoses.forEach(diagnostics::add);
        return gson.toJson(diagnostics);
    }

    private JsonObject toJson(SyntaxError syntaxError) {
        JsonObject diagnostic = new JsonObject();
        Optional.ofNullable(syntaxError.getErrorCode())
                .ifPresent(code -> diagnostic.add("code", new JsonPrimitive(code.getLabel())));
        Optional.ofNullable(syntaxError.getErrorSource())
                .ifPresent(es -> diagnostic.add("source", new JsonPrimitive(es.getText())));
        Optional.ofNullable(syntaxError.getLocation())
                .ifPresent(l -> diagnostic.add("location", gson.toJsonTree(l)));
        Optional.ofNullable(syntaxError.getSeverity())
                .ifPresent(s -> diagnostic.add("severity", new JsonPrimitive(s.name())));
        Optional.ofNullable(syntaxError.getSuggestion())
                .ifPresent(s -> diagnostic.add("suggestion", new JsonPrimitive(s)));
        Optional.ofNullable(syntaxError.getRelatedInformation())
                .ifPresent(ri -> diagnostic.add("related", gson.toJsonTree(ri)));
        return diagnostic;
    }
}
