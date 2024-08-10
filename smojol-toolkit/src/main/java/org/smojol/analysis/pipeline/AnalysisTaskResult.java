package org.smojol.analysis.pipeline;

public sealed interface AnalysisTaskResult permits AnalysisTaskResultOK, AnalysisTaskResultError {
    boolean isSuccess();
    static AnalysisTaskResult OK(Object detail) {
        return new AnalysisTaskResultOK(detail);
    }

    static AnalysisTaskResult OK() {
        return OK(new Object());
    }

    static AnalysisTaskResult ERROR(Exception detail) {
        return new AnalysisTaskResultError(detail);
    }
}
