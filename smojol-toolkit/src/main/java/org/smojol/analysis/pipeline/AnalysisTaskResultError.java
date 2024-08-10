package org.smojol.analysis.pipeline;

import lombok.Getter;

public final class AnalysisTaskResultError implements AnalysisTaskResult {
    @Getter private final Exception exception;

    public AnalysisTaskResultError(Exception exception) {
        this.exception = exception;
    }

    @Override
    public boolean isSuccess() {
        return false;
    }
}
