package org.smojol.analysis.pipeline;

import lombok.Getter;

public final class AnalysisTaskResultError implements AnalysisTaskResult {
    @Getter private final Exception exception;
    private final String task;

    public AnalysisTaskResultError(Exception exception) {
        this(exception, "<UNKNOWN_TASK>");
    }

    public AnalysisTaskResultError(Exception e, CommandLineAnalysisTask task) {
        this(e, task.name());
    }

    public AnalysisTaskResultError(Exception e, String task) {
        this.exception = e;
        this.task = task;
    }

    @Override
    public String toString() {
        return exception.toString();
    }

    @Override
    public boolean isSuccess() {
        return false;
    }
}
