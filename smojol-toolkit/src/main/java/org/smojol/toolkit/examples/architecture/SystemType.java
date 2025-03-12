package org.smojol.toolkit.examples.architecture;

import org.apache.commons.text.StringEscapeUtils;

public record SystemType(String systemType) {
    @Override
    public String toString() {
        return systemType;
    }

    public String toLabel() {
        if (systemType.trim().isEmpty()) return "NOTECH";
        return StringEscapeUtils.escapeJava(systemType
                .replace(" ", "_")
                .replace("/", "_")
                .replace("-", "_")
                .replace("+", "_AND_")
                .replace(",", "_")
                .replace("(", "_")
                .replace(")", "_"));
    }
}
