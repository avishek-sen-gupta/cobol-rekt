package org.smojol.toolkit.analysis.pipeline.config;

import org.smojol.common.ast.CobolTreeVisualiser;

import java.nio.file.Path;

public record RawASTOutputConfig(Path astOutputDir, String cobolParseTreeOutputPath, CobolTreeVisualiser visualiser) {
}
