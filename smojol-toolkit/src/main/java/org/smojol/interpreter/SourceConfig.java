package org.smojol.interpreter;

import java.io.File;

public record SourceConfig(String programName, File source, java.util.List<File> copyBookPaths, String cobolParseTreeOutputPath, String dialectJarPath) {
}
