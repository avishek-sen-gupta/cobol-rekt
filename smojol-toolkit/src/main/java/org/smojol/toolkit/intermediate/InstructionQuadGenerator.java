package org.smojol.toolkit.intermediate;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.pseudocode.*;
import org.smojol.common.vm.expression.ArithmeticExpressionVisitor;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.toolkit.ast.MoveFlowNode;

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

    public void quad(PseudocodeInstruction instruction) {
        FlowNode node = instruction.getNode();
        if (node instanceof MoveFlowNode n) {
            if (n.getFrom().literal() != null) {
                n.getFrom().literal().getText();
            } else if (n.getFrom().generalIdentifier() != null) {
                n.getFrom().generalIdentifier();
            }
//            new MoveQuad()
        }
    }

    public static List<PseudocodeInstruction> generalIdentifier(CobolParser.GeneralIdentifierContext generalIdentifierContext, SmojolSymbolTable symbolTable, SymbolReferenceBuilder symbolReferenceBuilder) {
        if (generalIdentifierContext.qualifiedDataName() != null) {
            String variableName = generalIdentifierContext.qualifiedDataName().variableUsageName().getText();
            SymbolReference reference = symbolTable.reference(variableName);
            if (generalIdentifierContext.qualifiedDataName().tableCall() != null) {
                List<CobolParser.ArithmeticExpressionContext> expressions = generalIdentifierContext.qualifiedDataName().tableCall().arithmeticExpression();
                ArithmeticExpressionVisitor arithmeticExpressionVisitor = new ArithmeticExpressionVisitor();
                List<CobolExpression> indexExpressions = expressions.stream().map(arithmeticExpressionVisitor::visitArithmeticExpression).toList();
                List<QuadSequence> sequences = indexExpressions.stream().map(expr -> {
                    ExpressionQuadrupleGeneratorVisitor visitor = new ExpressionQuadrupleGeneratorVisitor(new IncrementingIdProvider(), symbolTable, symbolReferenceBuilder);
                    expr.acceptDepthFirst(visitor);
                    QuadSequence quads = visitor.getQuads();
                    return quads;
                }).toList();
                List<SymbolReference> indexReferences = sequences.stream().map(QuadSequence::lastResult).toList();
            }
        }

        return ImmutableList.of();
    }
}
