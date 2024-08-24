package org.smojol.toolkit.analysis.defined;

import com.google.gson.annotations.Expose;
import lombok.Getter;
import org.smojol.common.ast.CallTarget;
import org.smojol.common.ast.ControlFlowNode;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.program.StaticCallTarget;
import org.smojol.common.program.TransferControlCollectorVisitor;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;

import java.util.List;

public class ProgramDependenciesTask implements AnalysisTask {
    private final FlowNode root;
    @Expose private final String programName;
    @Expose @Getter private final List<CallTarget> dependencies;

    public ProgramDependenciesTask(FlowNode root, String programName) {
        this.root = root;
        this.programName = programName;
        dependencies = transfersOfControl().stream().map(ControlFlowNode::callTarget).toList();
    }

    private List<ControlFlowNode> transfersOfControl() {
        TransferControlCollectorVisitor visitor = new TransferControlCollectorVisitor();
        root.accept(visitor, -1);
        List<ControlFlowNode> transfers = visitor.getTransfers();
        if (transfers.isEmpty()) {
            System.out.println(ConsoleColors.green("No transfers found!"));
            return transfers;
        }
        return transfers;
    }

    public boolean isEmpty() {
        return dependencies.isEmpty();
    }

    public List<CallTarget> staticDependencies() {
        return dependencies.stream().filter(dep -> dep.getClass() == StaticCallTarget.class).toList();
    }

    @Override
    public AnalysisTaskResult run() {
        return AnalysisTaskResult.OK(CommandLineAnalysisTask.BUILD_PROGRAM_DEPENDENCIES, dependencies);
    }
}
