package org.smojol.common.pseudocode;

import com.google.common.collect.ImmutableList;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.id.IdProvider;
import org.smojol.common.vm.expression.ArithmeticExpressionVisitor;
import org.smojol.common.vm.expression.CobolExpression;

import java.util.ArrayList;
import java.util.List;

import static org.smojol.common.pseudocode.CodeSentinelType.*;

public class PseudocodeInstructionGenerator {
    public static List<PseudocodeInstruction> generalIdentifier(CobolParser.GeneralIdentifierContext generalIdentifierContext) {
        if (generalIdentifierContext.qualifiedDataName() != null) {
            String variableName = generalIdentifierContext.qualifiedDataName().variableUsageName().getText();
            if (generalIdentifierContext.qualifiedDataName().tableCall() != null) {
                List<CobolParser.ArithmeticExpressionContext> expressions = generalIdentifierContext.qualifiedDataName().tableCall().arithmeticExpression();
                ArithmeticExpressionVisitor arithmeticExpressionVisitor = new ArithmeticExpressionVisitor();
                List<CobolExpression> cobolExpressions = expressions.stream().map(arithmeticExpressionVisitor::visitArithmeticExpression).toList();
                CobolExpression xx = cobolExpressions.getFirst();
                QuadrupleGeneratorVisitor visitor = new QuadrupleGeneratorVisitor(new IncrementingIdProvider());
                xx.acceptDepthFirst(visitor);
                List<InstructionQuad> quads = visitor.getQuads();
                quads.forEach(System.out::println);
            }
        }

        return ImmutableList.of();
    }

    public static List<PseudocodeInstruction> visiting(FlowNode node, IdProvider uuidProvider) {
        return ImmutableList.of(new PseudocodeInstruction(node, BODY, uuidProvider.next()));
    }

    public static PseudocodeInstruction entering(FlowNode node, IdProvider uuidProvider) {
        return new PseudocodeInstruction(node, ENTER, uuidProvider.next());
    }

    public static PseudocodeInstruction exiting(FlowNode node, IdProvider uuidProvider) {
        return new PseudocodeInstruction(node, EXIT, uuidProvider.next());
    }

    private static boolean isMarker(FlowNode node) {
        return node.type() == FlowNodeType.PARAGRAPH
                || node.type() == FlowNodeType.PARAGRAPH_NAME
                || node.type() == FlowNodeType.PARAGRAPHS
                || node.type() == FlowNodeType.SENTENCE
                || node.type() == FlowNodeType.COMPOSITE
                || node.type() == FlowNodeType.PROCEDURE_DIVISION_BODY
                || node.type() == FlowNodeType.SECTION_HEADER
                || node.type() == FlowNodeType.SECTION
                || node.type() == FlowNodeType.CONDITION_CLAUSE;
    }
}
