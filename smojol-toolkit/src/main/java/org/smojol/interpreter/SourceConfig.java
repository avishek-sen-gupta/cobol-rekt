package org.smojol.interpreter;

import java.io.File;

public record SourceConfig(String programName, File source, File[] copyBookPaths, String cobolParseTreeOutputPath, String dialectJarPath) {
}
