package org.smojol.interpreter;

import java.nio.file.Path;

public record RawASTOutputConfig(Path astOutputDir, org.smojol.common.flowchart.CobolTreeVisualiser visualiser) {
}
