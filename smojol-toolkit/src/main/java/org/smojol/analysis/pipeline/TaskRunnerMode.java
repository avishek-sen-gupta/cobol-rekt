package org.smojol.analysis.pipeline;

import com.google.common.collect.ImmutableList;
import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.smojol.analysis.DiagnosticRuntimeError;

import java.util.List;
import java.util.Map;

public interface TaskRunnerMode {
    TaskRunnerMode DIAGNOSTIC_MODE = new TaskRunnerMode() {
        @Override
        public CodeTaskRunner run(Map<String, List<SyntaxError>> errorMap, CodeTaskRunner codeTaskRunner) {
            return codeTaskRunner;
        }

        @Override
        public List<AnalysisTask> tasks(List<AnalysisTask> tasks) {
            return ImmutableList.of();
        }

        @Override
        public String toString() {
            return "DIAGNOSTIC";
        }
    };
    TaskRunnerMode PRODUCTION_MODE = new TaskRunnerMode() {
        @Override
        public CodeTaskRunner run(Map<String, List<SyntaxError>> errorMap, CodeTaskRunner codeTaskRunner) {
            if (!errorMap.isEmpty()) throw new DiagnosticRuntimeError(errorMap);
            return codeTaskRunner;
        }

        @Override
        public List<AnalysisTask> tasks(List<AnalysisTask> tasks) {
            return tasks;
        }

        @Override
        public String toString() {
            return "PRODUCTION";
        }
    };

    CodeTaskRunner run(Map<String, List<SyntaxError>> errorMap, CodeTaskRunner codeTaskRunner);

    List<AnalysisTask> tasks(List<AnalysisTask> tasks);
}
