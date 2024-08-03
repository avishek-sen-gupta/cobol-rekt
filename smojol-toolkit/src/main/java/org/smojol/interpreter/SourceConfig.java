package org.smojol.interpreter;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

public record SourceConfig(String programName, String sourceDir, java.util.List<File> copyBookPaths, String cobolParseTreeOutputPath, String dialectJarPath) {

    public File source() {
        return Paths.get(sourceDir, programName).toFile().getAbsoluteFile();
    }

    public Path sourcePath() {
        return Paths.get(sourceDir, programName);
    }
}
