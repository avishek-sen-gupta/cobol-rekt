package org.smojol.toolkit.analysis.task.analysis;

import com.google.gson.annotations.Expose;
import lombok.Getter;
import org.smojol.common.ast.CallTarget;
import org.smojol.common.ast.ExternalControlFlowNode;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.navigation.FlowNodeNavigator;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.util.ArrayList;
import java.util.List;

public class ProgramDependenciesTask implements AnalysisTask {
    private final FlowNode root;
    @Expose @Getter private final List<CallTarget> dependencies = new ArrayList<>();

    public ProgramDependenciesTask(FlowNode root) {
        this.root = root;
    }

    public boolean isEmpty() {
        return dependencies.isEmpty();
    }

    @Override
    public AnalysisTaskResult run() {
        List<FlowNode> allExternalCallNodes = new FlowNodeNavigator(root).findAllByCondition(fn -> fn instanceof ExternalControlFlowNode);
        List<CallTarget> callTargets = allExternalCallNodes.stream().map(t -> ((ExternalControlFlowNode) t).callTarget()).toList();
        dependencies.addAll(callTargets);

        return AnalysisTaskResult.OK(CommandLineAnalysisTask.BUILD_PROGRAM_DEPENDENCIES, dependencies);
    }
}
