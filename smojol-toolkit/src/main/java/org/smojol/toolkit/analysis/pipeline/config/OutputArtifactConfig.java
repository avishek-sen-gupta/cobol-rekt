package org.smojol.toolkit.analysis.pipeline.config;

import java.nio.file.Path;

public record OutputArtifactConfig(Path outputDir, String filename) {
    public String fullPath() {
        return outputDir.resolve(filename).toAbsolutePath().normalize().toString();
    }
}
