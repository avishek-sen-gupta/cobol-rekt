package org.smojol.analysis.pipeline;

public class AnalysisTaskResult {
    private Object detail;

    public AnalysisTaskResult(Object detail) {
        this.detail = detail;
    }

    static AnalysisTaskResult OK(Object detail) {
        return new AnalysisTaskResult(detail);
    }

    static AnalysisTaskResult OK() {
        return new AnalysisTaskResult(null);
    }

    public Object getDetail() {
        return detail;
    }
}
