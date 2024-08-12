package org.smojol.analysis.pipeline;

import lombok.Getter;

public final class AnalysisTaskResultOK implements AnalysisTaskResult {
    private final String task;
    @Getter private Object detail;

    public AnalysisTaskResultOK(Object detail) {
        this("<UNKNOWN_TASK>", detail);
    }

    public AnalysisTaskResultOK(String task, Object detail) {
        this.task = task;
        this.detail = detail;
    }

    @Override
    public boolean isSuccess() {
        return true;
    }

    @Override
    public String toString() {
        return task + ": OK";
    }
}
