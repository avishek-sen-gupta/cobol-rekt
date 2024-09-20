package org.smojol.toolkit.quad.generators;

import com.google.common.collect.ImmutableList;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.pseudocode.*;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.CobolExpressionBuilder;

import java.util.ArrayList;
import java.util.List;

public class GeneralIdentifierQuadGeneration {
    private final PseudocodeGraph graph;
    private final SmojolSymbolTable symbolTable;
    private final SymbolReferenceBuilder symbolReferenceBuilder;

    public GeneralIdentifierQuadGeneration(PseudocodeGraph graph, SmojolSymbolTable symbolTable, SymbolReferenceBuilder symbolReferenceBuilder) {
        this.graph = graph;
        this.symbolTable = symbolTable;
        this.symbolReferenceBuilder = symbolReferenceBuilder;
    }

    public QuadSequence run(CobolParser.GeneralIdentifierContext generalIdentifierContext) {
        if (generalIdentifierContext.qualifiedDataName() != null) {
            String variableName = generalIdentifierContext.qualifiedDataName().variableUsageName().getText();
            SymbolReference reference = symbolTable.reference(variableName);
            if (generalIdentifierContext.qualifiedDataName().tableCall() != null) {
                List<CobolParser.ArithmeticExpressionContext> expressions = generalIdentifierContext.qualifiedDataName().tableCall().arithmeticExpression();
                CobolExpressionBuilder expressionBuilder = new CobolExpressionBuilder();
                List<CobolExpression> indexExpressions = expressions.stream().map(expressionBuilder::arithmetic).toList();
                ExpressionQuadGenerator generator = new ExpressionQuadGenerator(symbolTable, symbolReferenceBuilder);
                List<QuadSequence> sequences = indexExpressions.stream().map(expr -> {
                    generator.build(expr);
                    return generator.getQuads();
                }).toList();

                List<SymbolReference> indexReferences = sequences.stream().map(QuadSequence::lastResult).toList();
                List<InstructionQuad> indexingQuads = recursivelyIndex(indexReferences, reference);
                QuadSequence allQuads = new QuadSequence(symbolTable);
                sequences.forEach(allQuads::add);
                indexingQuads.forEach(allQuads::add);
                return allQuads;
            } else {
                return new QuadSequence(symbolTable, ImmutableList.of(new InstructionQuad(reference, AbstractOperator.NO_OP)));
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
