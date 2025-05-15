package com.mojo.algorithms.task;

import lombok.Getter;
import org.apache.commons.lang3.exception.ExceptionUtils;

@Getter
public final class AnalysisTaskResultError implements AnalysisTaskResult {
    private final Exception exception;
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
        return ExceptionUtils.getStackTrace(exception);
    }

    @Override
    public boolean isSuccess() {
        return false;
    }
}
