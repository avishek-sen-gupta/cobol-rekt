package org.smojol.toolkit.analysis.defined;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.navigation.FlowNodeNavigator;
import org.smojol.common.pseudocode.*;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.intermediate.InstructionQuadGenerator;
import org.smojol.toolkit.analysis.pipeline.config.FlowASTOutputConfig;
import org.smojol.toolkit.ast.MoveFlowNode;
import org.smojol.toolkit.task.*;

import java.util.List;

public class GenerateIRTask implements AnalysisTask {
    private final FlowNode astRoot;
    private final CobolDataStructure dataStructures;
    private final FlowASTOutputConfig flowASTOutputConfig;

    public GenerateIRTask(FlowNode astRoot, CobolDataStructure dataStructures, FlowASTOutputConfig flowASTOutputConfig) {
        this.astRoot = astRoot;
        this.dataStructures = dataStructures;
        this.flowASTOutputConfig = flowASTOutputConfig;
    }

    @Override
    public AnalysisTaskResult run() {
        SymbolReferenceBuilder symbolReferenceBuilder = new SymbolReferenceBuilder(new IncrementingIdProvider());
        SmojolSymbolTable symbolTable = new SmojolSymbolTable(dataStructures, symbolReferenceBuilder);
        AnalysisTaskResult result = new BuildPseudocodeTask(astRoot, new UUIDProvider()).run();
        return switch (result) {
            case AnalysisTaskResultOK o -> updateGraph(o.getDetail(), symbolTable, symbolReferenceBuilder);
            case AnalysisTaskResultError e -> new AnalysisTaskResultError(e.getException(), CommandLineAnalysisTask.FLATTEN_FLOW_AST);
        };
    }

    private AnalysisTaskResult updateGraph(List<PseudocodeInstruction> instructions, SmojolSymbolTable symbolTable, SymbolReferenceBuilder symbolReferenceBuilder) {
        MoveFlowNode move = new FlowNodeNavigator(astRoot).findByType(MoveFlowNode.class);
        QuadSequence quads = new QuadSequence(symbolTable);
        InstructionQuadGenerator quadGenerator = new InstructionQuadGenerator(symbolReferenceBuilder, symbolTable);
//        for (PseudocodeInstruction instruction : instructions) {
//            quadGenerator.quad(instruction);
//        }
        quadGenerator.generalIdentifier(move.getTos().getFirst());

        return new AnalysisTaskResultOK(CommandLineAnalysisTask.FLATTEN_FLOW_AST.name());
    }
}
