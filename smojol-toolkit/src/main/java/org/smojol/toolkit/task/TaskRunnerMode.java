package org.smojol.toolkit.task;

import com.google.common.collect.ImmutableList;
import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.smojol.toolkit.analysis.error.DiagnosticRuntimeError;

import java.util.List;
import java.util.Map;

public interface TaskRunnerMode {
    TaskRunnerMode DIAGNOSTIC_MODE = new TaskRunnerMode() {
        @Override
        public Map<String, List<AnalysisTaskResult>> run(Map<String, List<SyntaxError>> errorMap, Map<String, List<AnalysisTaskResult>> results) {
            return results;
        }

        @Override
        public List<CommandLineAnalysisTask> tasks(List<CommandLineAnalysisTask> tasks) {
            return ImmutableList.of();
        }

        @Override
        public String toString() {
            return "DIAGNOSTIC";
        }
    };
    TaskRunnerMode PRODUCTION_MODE = new TaskRunnerMode() {
        @Override
        public Map<String, List<AnalysisTaskResult>> run(Map<String, List<SyntaxError>> errorMap, Map<String, List<AnalysisTaskResult>> taskResults) {
            if (!errorMap.isEmpty()) throw new DiagnosticRuntimeError(errorMap);
            return taskResults;
        }

        @Override
        public List<CommandLineAnalysisTask> tasks(List<CommandLineAnalysisTask> tasks) {
            return tasks;
        }

        @Override
        public String toString() {
            return "PRODUCTION";
        }
    };

    Map<String, List<AnalysisTaskResult>> run(Map<String, List<SyntaxError>> errorMap, Map<String, List<AnalysisTaskResult>> results);

    List<CommandLineAnalysisTask> tasks(List<CommandLineAnalysisTask> tasks);
}
