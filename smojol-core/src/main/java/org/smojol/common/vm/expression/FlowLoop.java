package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.ArrayList;
import java.util.List;

import static org.smojol.common.vm.expression.ConditionTestTime.AFTER;
import static org.smojol.common.vm.expression.ConditionTestTime.BEFORE;

public abstract class FlowLoop {
    public static List<Iteration> build(CobolParser.PerformTypeContext performTypeContext, CobolDataStructure dataStructures) {
        CobolExpressionBuilder builder = new CobolExpressionBuilder();
        if (performTypeContext.performTimes() != null)
            return ImmutableList.of(new Iteration(builder.literalOrIdentifier(performTypeContext.performTimes().integerLiteral(), performTypeContext.performTimes().generalIdentifier())));
        else if (performTypeContext.performUntil() != null) {
            if (performTypeContext.performUntil().performTestClause().BEFORE() != null)
                return ImmutableList.of(Iteration.whileLoop(builder.condition(performTypeContext.performUntil().condition(), dataStructures),
                        BEFORE));
            else
                return ImmutableList.of(Iteration.whileLoop(builder.condition(performTypeContext.performUntil().condition(), dataStructures),
                        AFTER));
        }
        CobolParser.PerformVaryingPhraseContext outerLoop = performTypeContext.performVarying().performVaryingClause().performVaryingPhrase();
        List<CobolParser.PerformVaryingPhraseContext> additionalNestedLoops = performTypeContext.performVarying().performVaryingClause().performAfter().stream().map(CobolParser.PerformAfterContext::performVaryingPhrase).toList();
        List<Iteration> allLoops = new ArrayList<>();
        allLoops.add(FlowLoop.asIteration(outerLoop, dataStructures));
        allLoops.addAll(additionalNestedLoops.stream().map(loop -> asIteration(loop, dataStructures)).toList());

        return allLoops;
    }

    private static Iteration asIteration(CobolParser.PerformVaryingPhraseContext outerLoop, CobolDataStructure dataStructures) {
        CobolExpressionBuilder builder = new CobolExpressionBuilder();
        return Iteration.withMaxValue(builder.literalOrIdentifier(outerLoop.performFrom().literal(), outerLoop.performFrom().generalIdentifier()),
                builder.literalOrIdentifier(outerLoop.performFrom().literal(), outerLoop.performFrom().generalIdentifier()),
                builder.condition(outerLoop.performUntil().condition(), dataStructures),
                new LoopUpdate(builder.literalOrIdentifier(outerLoop.performBy().literal(), outerLoop.performBy().generalIdentifier())),
                testTime(outerLoop.performUntil()));
    }

    private static ConditionTestTime testTime(CobolParser.PerformUntilContext performUntilContext) {
        if (performUntilContext.performTestClause() == null) return BEFORE;
        return performUntilContext.performTestClause().BEFORE() != null ? BEFORE : AFTER;
    }
}
