package org.smojol.toolkit.analysis.pipeline.config;

import java.nio.file.Path;
import org.smojol.common.ast.CobolTreeVisualiser;

public record RawASTOutputConfig(
    Path astOutputDir, String cobolParseTreeOutputPath, CobolTreeVisualiser visualiser) {}
