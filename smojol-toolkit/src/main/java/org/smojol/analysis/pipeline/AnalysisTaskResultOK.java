package org.smojol.analysis.pipeline;

import lombok.Getter;

public final class AnalysisTaskResultOK implements AnalysisTaskResult {
    @Getter private Object detail;

    public AnalysisTaskResultOK(Object detail) {
        this.detail = detail;
    }

    @Override
    public boolean isSuccess() {
        return true;
    }
}
