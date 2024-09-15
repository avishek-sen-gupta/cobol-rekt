package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.ArrayList;
import java.util.List;

import static org.smojol.common.vm.expression.ConditionTestTime.AFTER;
import static org.smojol.common.vm.expression.ConditionTestTime.BEFORE;

public class FlowIterationBuilder {
    public static List<FlowIteration> build(CobolParser.PerformTypeContext performTypeContext, CobolDataStructure dataStructures) {
        CobolExpressionBuilder builder = new CobolExpressionBuilder();
        if (performTypeContext.performTimes() != null)
            return ImmutableList.of(FlowIteration.times(builder.literalOrIdentifier(performTypeContext.performTimes().integerLiteral(), performTypeContext.performTimes().generalIdentifier())));
        else if (performTypeContext.performUntil() != null) {
            if (performTypeContext.performUntil().performTestClause() != null) {
                if (performTypeContext.performUntil().performTestClause().AFTER() != null)
                    return ImmutableList.of(FlowIteration.whileLoop(builder.condition(performTypeContext.performUntil().condition(), dataStructures),
                            AFTER));
                else
                    return ImmutableList.of(FlowIteration.whileLoop(builder.condition(performTypeContext.performUntil().condition(), dataStructures),
                            BEFORE));
            } else
                return ImmutableList.of(FlowIteration.whileLoop(builder.condition(performTypeContext.performUntil().condition(), dataStructures),
                        AFTER));

        }
        CobolParser.PerformVaryingPhraseContext outerLoop = performTypeContext.performVarying().performVaryingClause().performVaryingPhrase();
        List<CobolParser.PerformVaryingPhraseContext> additionalNestedLoops = performTypeContext.performVarying().performVaryingClause().performAfter().stream().map(CobolParser.PerformAfterContext::performVaryingPhrase).toList();
        List<FlowIteration> allLoops = new ArrayList<>();
        allLoops.add(FlowIterationBuilder.asIteration(outerLoop, dataStructures));
        allLoops.addAll(additionalNestedLoops.stream().map(loop -> asIteration(loop, dataStructures)).toList());

        return allLoops;
    }

    private static FlowIteration asIteration(CobolParser.PerformVaryingPhraseContext outerLoop, CobolDataStructure dataStructures) {
        CobolExpressionBuilder builder = new CobolExpressionBuilder();
        return FlowIteration.withCondition(builder.literalOrIdentifier(outerLoop.literal(), outerLoop.generalIdentifier()),
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
