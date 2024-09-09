package org.smojol.toolkit.analysis.defined;

import com.google.gson.annotations.Expose;
import lombok.Getter;
import org.smojol.common.ast.*;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.program.TransferControlCollectorVisitor;
import org.smojol.common.pseudocode.PseudocodeInstruction;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

public class ProgramDependenciesTask implements AnalysisTask {
    java.util.logging.Logger LOGGER = Logger.getLogger(ProgramDependenciesTask.class.getName());
    private final FlowNode root;
    @Expose
    private final String programName;
    @Expose @Getter private final List<CallTarget> dependencies = new ArrayList<>();

    public ProgramDependenciesTask(FlowNode root, String programName) {
        this.root = root;
        this.programName = programName;
    }

    private List<ExternalControlFlowNode> transfersOfControl() {
        TransferControlCollectorVisitor visitor = new TransferControlCollectorVisitor();
        root.accept(visitor, -1);
        List<ExternalControlFlowNode> transfers = visitor.getTransfers();
        if (transfers.isEmpty()) {
            LOGGER.warning(ConsoleColors.green("No transfers found!"));
            return transfers;
        }
        return transfers;
    }

    public boolean isEmpty() {
        return dependencies.isEmpty();
    }

    @Override
    public AnalysisTaskResult run() {
        List<PseudocodeInstruction> instructions = new BuildPseudocodeTask(root, new UUIDProvider()).run();
        List<PseudocodeInstruction> allTransfers = instructions.stream().filter(ins -> ins.isBody() && ins.getNode() instanceof ExternalControlFlowNode).toList();
        List<CallTarget> resolvedCallTargets = allTransfers.stream().map(t -> ((ExternalControlFlowNode) t.getNode()).callTarget().resolve(t, instructions)).toList();
        dependencies.addAll(resolvedCallTargets);

        return AnalysisTaskResult.OK(CommandLineAnalysisTask.BUILD_PROGRAM_DEPENDENCIES, dependencies);
    }
}
