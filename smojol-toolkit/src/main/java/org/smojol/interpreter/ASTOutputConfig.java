package org.smojol.interpreter;

import java.nio.file.Path;

public record ASTOutputConfig(Path astOutputDir, org.smojol.common.flowchart.CobolTreeVisualiser visualiser) {
}
