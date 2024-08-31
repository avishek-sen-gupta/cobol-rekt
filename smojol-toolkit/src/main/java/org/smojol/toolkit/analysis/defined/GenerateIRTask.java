package org.smojol.toolkit.analysis.defined;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.navigation.FlowNodeNavigator;
import org.smojol.common.pseudocode.PseudocodeInstruction;
import org.smojol.common.pseudocode.PseudocodeInstructionGenerator;
import org.smojol.toolkit.analysis.pipeline.config.FlowASTOutputConfig;
import org.smojol.toolkit.ast.MoveFlowNode;
import org.smojol.toolkit.task.*;

import java.util.List;

public class GenerateIRTask implements AnalysisTask {
    private final FlowNode astRoot;
    private final FlowASTOutputConfig flowASTOutputConfig;

    public GenerateIRTask(FlowNode astRoot, FlowASTOutputConfig flowASTOutputConfig) {
        this.astRoot = astRoot;
        this.flowASTOutputConfig = flowASTOutputConfig;
    }

    @Override
    public AnalysisTaskResult run() {
        AnalysisTaskResult result = new BuildPseudocodeTask(astRoot, new UUIDProvider()).run();
        return switch (result) {
            case AnalysisTaskResultOK o -> updateGraph();
            case AnalysisTaskResultError e -> new AnalysisTaskResultError(e.getException(), CommandLineAnalysisTask.FLATTEN_FLOW_AST);
        };
    }

    private AnalysisTaskResult updateGraph() {
        MoveFlowNode move = new FlowNodeNavigator(astRoot).findByType(MoveFlowNode.class);
        List<PseudocodeInstruction> instructions = PseudocodeInstructionGenerator.generalIdentifier(move.getTos().getFirst());

        return new AnalysisTaskResultOK(CommandLineAnalysisTask.FLATTEN_FLOW_AST.name());
    }
}
