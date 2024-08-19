package org.smojol.toolkit.analysis.pipeline.config;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

public record SourceConfig(String programName, String sourceDir, java.util.List<File> copyBookPaths, String dialectJarPath) {

    public File source() {
        return Paths.get(sourceDir, programName).toFile().getAbsoluteFile();
    }

    public Path sourcePath() {
        return Paths.get(sourceDir, programName);
    }
}
