package org.smojol.toolkit.quad;

import lombok.Getter;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.pseudocode.*;
import org.smojol.toolkit.ast.*;
import org.smojol.toolkit.quad.generators.*;

import java.util.ArrayList;
import java.util.List;

public class QuadSequenceGenerator {
    private final PseudocodeGraph graph;
    private final SymbolReferenceBuilder symbolReferenceBuilder;
    private final SmojolSymbolTable symbolTable;
    @Getter
    private final List<InstructionQuad> quads = new ArrayList<>();

    public QuadSequenceGenerator(PseudocodeGraph graph, SymbolReferenceBuilder symbolReferenceBuilder, SmojolSymbolTable symbolTable) {
        this.graph = graph;
        this.symbolReferenceBuilder = symbolReferenceBuilder;
        this.symbolTable = symbolTable;
    }

    public QuadSequence quadSequence(PseudocodeInstruction instruction) {
        FlowNode node = instruction.getNode();
        QuadGeneration quadGeneration = quadGenerator(graph, node);
        return switch (instruction.codeSentinelType()) {
            case ENTER -> quadGeneration.enter(instruction);
            case EXIT -> quadGeneration.exit(instruction);
            case BODY -> quadGeneration.body(instruction);
        };
    }

    private QuadGeneration quadGenerator(PseudocodeGraph graph, FlowNode node) {
        return switch (node) {
            case MoveFlowNode n -> new MoveQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case ComputeFlowNode n -> new ComputeQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case MultiplyFlowNode n -> new MultiplyQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case AddFlowNode n -> new AddQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case SubtractFlowNode n -> new SubtractQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case DivideFlowNode n -> new DivideQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case IfFlowNode n -> new IfQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case IfThenFlowNode n -> new IfThenQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case IfElseFlowNode n -> new IfElseQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case GoToFlowNode n -> new GoToQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case PerformProcedureFlowNode n ->
                    new PerformProcedureQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case PerformInlineFlowNode n ->
                    new PerformInlineQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case PerformTestFlowNode n ->
                    new PerformTestQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case NextSentenceFlowNode n ->
                    new NextSentenceQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case SentenceFlowNode n -> new SentenceQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case SectionFlowNode n -> new SectionQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case ProcedureDivisionBodyFlowNode n ->
                    new ProcedureDivisioneQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case ParagraphFlowNode n -> new ParagraphQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            case ParagraphsFlowNode n -> new ParagraphsQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
            default -> new NoOpQuadGeneration(graph, symbolTable, symbolReferenceBuilder);
        };
    }
}
