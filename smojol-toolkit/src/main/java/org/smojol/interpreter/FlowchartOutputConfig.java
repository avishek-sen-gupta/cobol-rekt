package org.smojol.interpreter;

import java.nio.file.Path;

public record FlowchartOutputConfig(Path astOutputDir, Path dotFileOutputDir, Path imageOutputDir,
                                    FlowchartGenerationStrategy flowchartGenerationStrategy) {
}
