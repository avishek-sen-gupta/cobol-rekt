package org.smojol.toolkit.interpreter.interpreter;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.PrimitiveCobolExpression;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.type.TypedRecord;
import org.smojol.toolkit.ast.SubtractFlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.reference.DeepReferenceBuilder;
import org.smojol.common.vm.structure.CobolOperation;

import java.util.List;
import java.util.stream.Stream;

import static com.google.common.collect.Streams.zip;

public class SubtractOperation implements CobolOperation {
    private final SubtractFlowNode subtract;

    public SubtractOperation(SubtractFlowNode subtract) {
        this.subtract = subtract;
    }

    public void run(CobolDataStructure cobolDataStructure) {
//        List<CobolParser.SubtractMinuendContext> lhses = subtract.getMinuends();
//        List<CobolParser.SubtractSubtrahendContext> rhses = subtract.getSubtrahends();
        DeepReferenceBuilder builder = new DeepReferenceBuilder();
//        lhses.forEach(lhs -> rhses.forEach(rhs -> builder.getReference(lhs, cobolDataStructure).resolve().subtract(builder.getReference(rhs, cobolDataStructure))));
        CobolExpression subtrahendSum = subtract.getSubtrahendExpressions().stream().map(srcExpr -> srcExpr.evaluate(cobolDataStructure)).reduce(new PrimitiveCobolExpression(TypedRecord.typedNumber(0)), (sum, currentExpr) -> sum.add(currentExpr, cobolDataStructure));
        List<CobolExpression> differences = subtract.getMinuendExpressions().stream().map(minuend -> minuend.evaluate(cobolDataStructure).subtract(subtrahendSum, cobolDataStructure)).toList();
        if (subtract.isGiving()) {
            subtract.getDestinationExpressions().forEach(to -> builder.getReference(to, cobolDataStructure).set(builder.getReference(differences.getFirst(), cobolDataStructure)));
        } else {
            List<Pair<CobolReference, CobolReference>> srcDestPairs = zip(differences.stream(), subtract.getDestinationExpressions().stream(), (src, dest) -> (Pair<CobolReference, CobolReference>) ImmutablePair.of(builder.getReference(src, cobolDataStructure), builder.getReference(dest, cobolDataStructure))).toList();
            srcDestPairs.forEach(p -> p.getRight().set(p.getLeft()));
        }
    }
}
