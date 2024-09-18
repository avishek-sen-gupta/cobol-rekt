package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.TypedRecord;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static com.google.common.collect.Streams.zip;
import static org.smojol.common.list.ConsCar.head;
import static org.smojol.common.list.ConsCar.tail;

public class EvaluateBreaker {
    private final CobolDataStructure dataStructures;
    private final StackFrames staticFrameContext;
    private final FlowNode parent;
    private final FlowNodeService nodeService;
    private final CobolExpressionBuilder cobolExpressionBuilder = new CobolExpressionBuilder();
    private final List<CobolExpression> evaluationSubjects = new ArrayList<>();

    public EvaluateBreaker(StackFrames staticFrameContext, FlowNode parent, FlowNodeService nodeService) {
        this.dataStructures = nodeService.getDataStructures();
        this.staticFrameContext = staticFrameContext;
        this.parent = parent;
        this.nodeService = nodeService;
    }

    public ExpandedEvaluation decompose(CobolParser.EvaluateStatementContext whenStatement) {
        List<FlowNode> elseBody = whenStatement.evaluateWhenOther() != null
                ? whenStatement.evaluateWhenOther().conditionalStatementCall().stream().map(stmt -> nodeService.node(stmt, parent, this.staticFrameContext)).toList()
                : ImmutableList.of();
        evaluationSubjects.add(selectionSubject(whenStatement.evaluateSelect(), dataStructures));
        evaluationSubjects.addAll(whenStatement.evaluateAlsoSelect().stream().map(subj -> selectionSubject(subj.evaluateSelect(), dataStructures)).toList());
        List<TestActionPair> whenPhraseFlowNodes = whenStatement.evaluateWhenPhrase().stream().map(this::conditionGroup).toList();
        return new ExpandedEvaluation(evaluationSubjects, whenPhraseFlowNodes, elseBody);
    }

    public CobolExpression selectionSubject(CobolParser.EvaluateSelectContext selectionSubject, CobolDataStructure dataStructures) {
        if (selectionSubject.arithmeticExpression() != null)
            return cobolExpressionBuilder.arithmetic(selectionSubject.arithmeticExpression());
        return cobolExpressionBuilder.condition(selectionSubject.condition(), dataStructures);
    }

    public TestActionPair conditionGroup(CobolParser.EvaluateWhenPhraseContext whenPhrase) {
        CobolExpression condition = recursiveOr(head(whenPhrase.evaluateWhen()), tail(whenPhrase.evaluateWhen()));
        List<FlowNode> body = whenPhrase.conditionalStatementCall().stream().map(stmt -> nodeService.node(stmt, parent, this.staticFrameContext)).toList();
        return new TestActionPair(condition, body);
    }

    private CobolExpression recursiveOr(Optional<CobolParser.EvaluateWhenContext> head, List<CobolParser.EvaluateWhenContext> remaining) {
        if (remaining.isEmpty()) return mustHaveConditions(head.get());
        return new OrExpression(mustHaveConditions(head.get()), recursiveOr(head(remaining), tail(remaining)));
    }

    private CobolExpression mustHaveConditions(CobolParser.EvaluateWhenContext ctx) {
        List<CobolExpression> conditions = new ArrayList<>();
        conditions.add(condition(ctx.evaluateCondition(), evaluationSubjects.getFirst(), dataStructures));
        List<ImmutablePair<CobolParser.EvaluateAlsoConditionContext, CobolExpression>> subjectConditionPairs = zip(ctx.evaluateAlsoCondition().stream(), tail(evaluationSubjects).stream(), (a, b) -> ImmutablePair.of(a, b)).toList();
        conditions.addAll(subjectConditionPairs.stream().map(p -> condition(p.getLeft().evaluateCondition(), p.getRight(), dataStructures)).toList());
        return recursiveAnd(head(conditions), tail(conditions));
    }

    private CobolExpression recursiveAnd(Optional<CobolExpression> head, List<CobolExpression> remaining) {
        if (remaining.isEmpty()) return head.get();
        return new AndExpression(head.get(), recursiveAnd(head(remaining), tail(remaining)));
    }

    private CobolExpression condition(CobolParser.EvaluateConditionContext evaluateConditionContext, CobolExpression associatedSubject, CobolDataStructure dataStructures) {
        if (evaluateConditionContext.condition() != null)
            return new SimpleConditionExpression(associatedSubject, new RelationExpression(RelationalOperation.EQUAL, new CobolExpressionBuilder().condition(evaluateConditionContext.condition(), dataStructures)));
        else if (evaluateConditionContext.booleanLiteral() != null)
            return new SimpleConditionExpression(associatedSubject, new PrimitiveCobolExpression(TypedRecord.typedBoolean(Boolean.parseBoolean(evaluateConditionContext.booleanLiteral().getText()))));
        else if (evaluateConditionContext.ANY() != null)
            return new SimpleConditionExpression(associatedSubject, new PrimitiveCobolExpression(TypedRecord.TRUE));
        List<CobolExpression> args = ImmutableList.of(associatedSubject, cobolExpressionBuilder.arithmetic(evaluateConditionContext.evaluateValue().arithmeticExpression()),
                cobolExpressionBuilder.arithmetic(evaluateConditionContext.evaluateThrough().evaluateValue().arithmeticExpression()));
        FunctionCallExpression isInRangeCall = new FunctionCallExpression("isInRange", args);
        return evaluateConditionContext.NOT() != null ? new NotExpression(isInRangeCall) : isInRangeCall;
    }
}
