package org.smojol.ast;

import com.google.gson.annotations.Expose;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.flowchart.ConsoleColors;

import java.util.List;

public class ProgramDependencies {
    private final FlowNode root;
    @Expose private final String programName;
    @Expose private final List<CallTarget> dependencies;

    public ProgramDependencies(FlowNode root, String programName) {
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
}
