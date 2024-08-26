package org.smojol.toolkit.task;

import lombok.Getter;

public final class AnalysisTaskResultOK implements AnalysisTaskResult {
    @Getter private final String task;
    private final Object detail;

    public AnalysisTaskResultOK(Object detail) {
        this("<UNKNOWN_TASK>", detail);
    }

    public AnalysisTaskResultOK(String task, Object detail) {
        this.task = task;
        this.detail = detail;
    }

    public <T> T getDetail() {
        return (T) detail;
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
