package com.mojo.algorithms.task;

import lombok.Getter;

public final class AnalysisTaskResultOK implements AnalysisTaskResult {
    @Getter private final String task;
    private final Object detail;

    public AnalysisTaskResultOK(String task, Object detail) {
        this.task = task;
        this.detail = detail;
    }

    public AnalysisTaskResultOK(String task) {
        this(task, new Object());
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
