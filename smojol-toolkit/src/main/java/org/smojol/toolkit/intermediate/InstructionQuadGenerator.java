package org.smojol.toolkit.intermediate;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.pseudocode.*;
import org.smojol.common.vm.expression.*;
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

    public QuadSequence quad(PseudocodeInstruction instruction) {
        FlowNode node = instruction.getNode();
        return switch (node) {
            case MoveFlowNode n -> new MoveQuadGeneration(n).run();
            case ComputeFlowNode n -> new ComputeQuadGeneration(n).run();
            case MultiplyFlowNode n -> new MultiplyQuadGeneration(n).run();
            case AddFlowNode n -> new AddQuadGeneration(n).run();
            case SubtractFlowNode n -> new SubtractQuadGeneration(n).run();
            case DivideFlowNode n -> new DivideQuadGeneration(n).run();
            case IfFlowNode n -> new IfQuadGeneration(n).run();
            case IfThenFlowNode n -> new IfThenQuadGeneration(n).run();
            case IfElseFlowNode n -> new IfElseQuadGeneration(n).run();
            case GoToFlowNode n -> new GoToQuadGeneration(n).run();
            case NextSentenceFlowNode n -> new NextSentenceQuadGeneration(n).run();
            default -> throw new UnsupportedOperationException("Unsupported node type: " + node.getClass());
        };
    }

    public QuadSequence generalIdentifier(CobolParser.GeneralIdentifierContext generalIdentifierContext) {
        if (generalIdentifierContext.qualifiedDataName() != null) {
            String variableName = generalIdentifierContext.qualifiedDataName().variableUsageName().getText();
            SymbolReference reference = symbolTable.reference(variableName);
            if (generalIdentifierContext.qualifiedDataName().tableCall() != null) {
                List<CobolParser.ArithmeticExpressionContext> expressions = generalIdentifierContext.qualifiedDataName().tableCall().arithmeticExpression();
                ArithmeticExpressionVisitor arithmeticExpressionVisitor = new ArithmeticExpressionVisitor();
                List<CobolExpression> indexExpressions = expressions.stream().map(arithmeticExpressionVisitor::visitArithmeticExpression).toList();
                List<QuadSequence> sequences = indexExpressions.stream().map(expr -> {
                    ExpressionQuadGenerator visitor = new ExpressionQuadGenerator(symbolTable, symbolReferenceBuilder);
                    visitor.build(expr);
                    QuadSequence quads = visitor.getQuads();
                    return quads;
                }).toList();

                List<SymbolReference> indexReferences = sequences.stream().map(QuadSequence::lastResult).toList();
                List<InstructionQuad> indexingQuads = recursivelyIndex(indexReferences, reference);
                QuadSequence allQuads = new QuadSequence(symbolTable);
                sequences.forEach(allQuads::add);
                indexingQuads.forEach(allQuads::add);
                System.out.println("DONE");
                return allQuads;
            } else {
                return new QuadSequence(symbolTable);
            }
        }

        throw new UnsupportedOperationException("specialRegister and functionCall variants are not supported yet");
    }

    private List<InstructionQuad> recursivelyIndex(List<SymbolReference> indexReferences, SymbolReference currentIndexee) {
        if (indexReferences.isEmpty()) return ImmutableList.of();
        SymbolReference index = indexReferences.getFirst();
        List<InstructionQuad> q = new ArrayList<>();
        SymbolReference nextIndexee = symbolReferenceBuilder.intermediateSymbolReference();
        q.add(new InstructionQuad(nextIndexee, AbstractOperator.INDEX, currentIndexee, index));
        q.addAll(recursivelyIndex(indexReferences.subList(1, indexReferences.size()), nextIndexee));
        return q;
    }
}
