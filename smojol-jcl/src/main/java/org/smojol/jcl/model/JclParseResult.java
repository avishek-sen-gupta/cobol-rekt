package org.smojol.jcl.model;

import com.google.gson.JsonObject;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;

/**
 * Represents the result of parsing a JCL file.
 * Contains the status, parsed JCL structure, and any potential errors.
 */
@Data
@ToString
@EqualsAndHashCode
public class JclParseResult {
    private String status;
    private String file;
    private JsonObject jcl;
    private String error;
    private String message;
    private String type;

    /**
     * Check if the parsing was successful.
     *
     * @return true if parsing succeeded, false otherwise
     */
    public boolean isSuccess() {
        return "success".equalsIgnoreCase(status);
    }

    /**
     * Check if the parsing failed.
     *
     * @return true if parsing failed, false otherwise
     */
    public boolean isError() {
        return "error".equalsIgnoreCase(status);
    }

    /**
     * Get the error details if parsing failed.
     *
     * @return formatted error message
     */
    public String getErrorDetails() {
        if (isSuccess()) {
            return null;
        }
        return String.format("Error: %s - %s (Type: %s)", error, message, type);
    }
}
