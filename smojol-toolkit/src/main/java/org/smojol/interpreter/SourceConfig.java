package org.smojol.interpreter;

import lombok.Getter;

import java.io.File;

public record SourceConfig(File source, File[] copyBookPaths, String cobolParseTreeOutputPath, String dialectJarPath) {
}
