package org.smojol.interpreter;

import org.smojol.common.ast.CobolTreeVisualiser;

import java.nio.file.Path;

public record RawASTOutputConfig(Path astOutputDir, CobolTreeVisualiser visualiser) {
}
