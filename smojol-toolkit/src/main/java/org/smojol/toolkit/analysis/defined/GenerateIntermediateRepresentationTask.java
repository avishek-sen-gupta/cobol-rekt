package org.smojol.toolkit.analysis.defined;

import com.mojo.woof.Neo4JDriverBuilder;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.id.IdProvider;
import org.smojol.common.navigation.FlowNodeNavigator;
import org.smojol.common.pseudocode.*;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.intermediate.InstructionQuadGenerator;
import org.smojol.toolkit.ast.MoveFlowNode;
import org.smojol.toolkit.task.*;

import java.util.List;

public class GenerateIntermediateRepresentationTask implements AnalysisTask {
    private final FlowNode astRoot;
    private final CobolDataStructure dataStructures;
    private final IdProvider idProvider;
    private final Neo4JDriverBuilder neo4JDriverBuilder;

    public GenerateIntermediateRepresentationTask(FlowNode astRoot, CobolDataStructure dataStructures, IdProvider idProvider, Neo4JDriverBuilder neo4JDriverBuilder) {
        this.astRoot = astRoot;
        this.dataStructures = dataStructures;
        this.idProvider = idProvider;
        this.neo4JDriverBuilder = neo4JDriverBuilder;
    }

    @Override
    public AnalysisTaskResult run() {
        SymbolReferenceBuilder symbolReferenceBuilder = new SymbolReferenceBuilder(new IncrementingIdProvider());
        SmojolSymbolTable symbolTable = new SmojolSymbolTable(dataStructures, symbolReferenceBuilder);
        AnalysisTaskResult result = new BuildPseudocodeGraphTask(astRoot, neo4JDriverBuilder, idProvider).run();
        return switch (result) {
            case AnalysisTaskResultOK o -> quads(o.getDetail(), symbolTable, symbolReferenceBuilder);
            case AnalysisTaskResultError e -> new AnalysisTaskResultError(e.getException(), CommandLineAnalysisTask.BUILD_PSEUDOCODE_GRAPH);
        };
    }

    private AnalysisTaskResult quads(List<PseudocodeInstruction> instructions, SmojolSymbolTable symbolTable, SymbolReferenceBuilder symbolReferenceBuilder) {
        MoveFlowNode move = new FlowNodeNavigator(astRoot).findByType(MoveFlowNode.class);
        QuadSequence quads = new QuadSequence(symbolTable);
        InstructionQuadGenerator quadGenerator = new InstructionQuadGenerator(symbolReferenceBuilder, symbolTable);

        for (PseudocodeInstruction instruction : instructions) {
            quadGenerator.quad(instruction);
        }
        quadGenerator.generalIdentifier(move.getTos().getFirst());

        return new AnalysisTaskResultOK(CommandLineAnalysisTask.BUILD_PSEUDOCODE_GRAPH.name());
    }
}
