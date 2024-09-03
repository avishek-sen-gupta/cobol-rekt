package org.smojol.toolkit.intermediate;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.pseudocode.*;
import org.smojol.toolkit.ast.*;
import org.smojol.toolkit.intermediate.generators.*;

import java.util.ArrayList;
import java.util.List;

public class InstructionQuadGenerator {
    private final SymbolReferenceBuilder symbolReferenceBuilder;
    private final SmojolSymbolTable symbolTable;
    @Getter
    private final List<InstructionQuad> quads = new ArrayList<>();

    public InstructionQuadGenerator(SymbolReferenceBuilder symbolReferenceBuilder, SmojolSymbolTable symbolTable) {
        this.symbolReferenceBuilder = symbolReferenceBuilder;
        this.symbolTable = symbolTable;
    }

    public QuadSequence quad(PseudocodeInstruction instruction, PseudocodeGraph graph) {
        FlowNode node = instruction.getNode();
        return switch (node) {
            case MoveFlowNode n -> new MoveQuadGeneration(graph, symbolTable, symbolReferenceBuilder).run(n);
            case ComputeFlowNode n -> new ComputeQuadGeneration(n).run();
            case MultiplyFlowNode n -> new MultiplyQuadGeneration(n).run();
            case AddFlowNode n -> new AddQuadGeneration(n).run();
            case SubtractFlowNode n -> new SubtractQuadGeneration(n).run();
            case DivideFlowNode n -> new DivideQuadGeneration(n).run();
            case IfFlowNode n -> new IfQuadGeneration(n).run();
            case IfThenFlowNode n -> new IfThenQuadGeneration(n).run();
            case IfElseFlowNode n -> new IfElseQuadGeneration(n).run();
            case GoToFlowNode n -> new GoToQuadGeneration(n).run();
            case PerformProcedureFlowNode n -> new PerformProcedureQuadGeneration(n).run();
            case PerformInlineFlowNode n -> new PerformInlineQuadGeneration(n).run();
            case PerformTestFlowNode n -> new PerformTestQuadGeneration(n).run();
            case NextSentenceFlowNode n -> new NextSentenceQuadGeneration(n).run();
            case SentenceFlowNode n -> new SentenceQuadGeneration(n).run();
            case SectionFlowNode n -> new SectionQuadGeneration(n).run();
            case ProcedureDivisionBodyFlowNode n -> new ProcedureDivisioneQuadGeneration(n).run();
            case ParagraphFlowNode n -> new ParagraphQuadGeneration(n).run();
            case ParagraphsFlowNode n -> new ParagraphsQuadGeneration(n).run();
            default -> new QuadSequence(symbolTable, ImmutableList.of(InstructionQuad.noOp()));
        };
    }
}
