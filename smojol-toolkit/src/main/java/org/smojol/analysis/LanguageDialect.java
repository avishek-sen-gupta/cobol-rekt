package org.smojol.analysis;

import org.eclipse.lsp.cobol.common.AnalysisConfig;
import org.eclipse.lsp.cobol.common.copybook.CopybookProcessingMode;

public enum LanguageDialect {
    COBOL {
        @Override
        AnalysisConfig analysisConfig(String dialectConfigJarPath) {
            return AnalysisConfig.defaultConfig(CopybookProcessingMode.ENABLED);
        }
    }, IDMS {
        @Override
        AnalysisConfig analysisConfig(String dialectConfigJarPath) {
            return AnalysisConfig.idmsConfig(dialectConfigJarPath, CopybookProcessingMode.ENABLED);
        }
    };

    abstract AnalysisConfig analysisConfig(String dialectConfigJarPath);
}
