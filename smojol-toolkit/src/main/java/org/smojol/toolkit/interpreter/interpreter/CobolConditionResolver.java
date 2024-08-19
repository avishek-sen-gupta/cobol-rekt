package org.smojol.toolkit.interpreter.interpreter;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.interpreter.*;
import org.smojol.toolkit.ast.GenericOnClauseFlowNode;
import org.smojol.toolkit.ast.IfFlowNode;
import org.smojol.toolkit.ast.SearchWhenFlowNode;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.flowchart.ConsoleColors;

import static org.smojol.common.ast.NodeText.formatted;

public class CobolConditionResolver implements ConditionResolver {
    public static ConditionResolver ALWAYS_YES = new CobolConditionResolver(AlwaysBooleanResolver.ALWAYS_TRUE, AlwaysBooleanResolver.ALWAYS_TRUE, AlwaysBooleanResolver.ALWAYS_TRUE);
    public static ConditionResolver ALWAYS_NO = new CobolConditionResolver(AlwaysBooleanResolver.ALWAYS_FALSE, AlwaysBooleanResolver.ALWAYS_FALSE, AlwaysBooleanResolver.ALWAYS_FALSE);
    public static ConditionResolver CONSOLE_RESOLVER = new CobolConditionResolver(ConsoleInputResolver.CONSOLE_INPUT_RESOLVER, ConsoleInputResolver.CONSOLE_INPUT_RESOLVER, ConsoleInputResolver.CONSOLE_INPUT_RESOLVER);
    public static ConditionResolver EVALUATING_RESOLVER = new CobolConditionResolver(ExpressionEvaluationResolver.EXPRESSION_EVALUATOR, AlwaysBooleanResolver.ALWAYS_TRUE, AlwaysBooleanResolver.ALWAYS_TRUE);

    private final BooleanResolver ifResolver;
    private final BooleanResolver whenResolver;
    private final BooleanResolver onResolver;

    public CobolConditionResolver(BooleanResolver ifResolver, BooleanResolver whenResolver, BooleanResolver onResolver) {
        this.ifResolver = ifResolver;
        this.whenResolver = whenResolver;
        this.onResolver = whenResolver;
    }

    @Override
    public boolean resolveIf(FlowNode node, FlowNodeService nodeService) {
        IfFlowNode ifNode = (IfFlowNode) node;
        boolean result = ifResolver.resolve(node, ifNode.getCondition(), nodeService);
        System.out.println(ConsoleColors.green("Resolved " + formatted(node.label()) + " condition to " + result + "..."));
        return result;
    }

    @Override
    public boolean resolveWhen(FlowNode whenPhrase, FlowNodeService nodeService) {
        SearchWhenFlowNode whenNode = (SearchWhenFlowNode) whenPhrase;
        boolean result = whenResolver.resolve(whenPhrase, (CobolParser.ConditionContext) whenNode.getCondition().getExecutionContext(), nodeService);
        System.out.println(ConsoleColors.green("Resolved WHEN condition: " + formatted(whenPhrase.label()) + " to " + result + "..."));
        return result;
    }

    @Override
    public boolean resolveOn(FlowNode node, FlowNodeService nodeService) {
        GenericOnClauseFlowNode onNode = (GenericOnClauseFlowNode) node;
        boolean result = onResolver.resolve(node, null, nodeService);
        System.out.println(ConsoleColors.green("Resolved " + formatted(node.label()) + " condition to " + result + "..."));
        return result;
    }
}
