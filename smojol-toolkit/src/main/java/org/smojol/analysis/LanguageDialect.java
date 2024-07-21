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

    public static LanguageDialect dialect(String dialectAsString) {
        if (dialectAsString == null) return COBOL;
        else if (dialectAsString.equals(IDMS.name())) return IDMS;
        else if (dialectAsString.equals(COBOL.name())) return COBOL;
        else return COBOL;
    }

    abstract AnalysisConfig analysisConfig(String dialectConfigJarPath);
}
