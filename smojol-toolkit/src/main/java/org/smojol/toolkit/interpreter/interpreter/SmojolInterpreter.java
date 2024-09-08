package org.smojol.toolkit.interpreter.interpreter;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.antlr.v4.runtime.RuleContext;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.interpreter.*;
import org.smojol.common.vm.stack.ExecutionContext;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.CobolOperations;
import org.smojol.toolkit.ast.*;

import java.util.List;
import java.util.logging.Logger;

import static org.smojol.common.flowchart.ConsoleColors.*;
import static org.smojol.common.ast.NodeText.formatted;

// TODO: Notify listeners of visit() in a more consistent manner
public class SmojolInterpreter implements CobolInterpreter {
    java.util.logging.Logger LOGGER = Logger.getLogger(SmojolInterpreter.class.getName());

    @Getter private final StackFrames runtimeStackFrames;
    private final ExecuteCondition condition;
    private final ConditionResolver conditionResolver;
    private final Breakpointer breakpointer;
    private final ExecutionInterceptors interceptors;
    private final List<ExecutionInterceptor> otherInterceptors;
    private final CobolOperations operations;
    private final ExecutionListener listeners;

    public SmojolInterpreter(StackFrames runtimeStackFrames, ExecuteCondition condition, ConditionResolver conditionResolver, Breakpointer bp, List<ExecutionInterceptor> otherInterceptors, ExecutionListener listeners, CobolOperations operations) {
        this.runtimeStackFrames = runtimeStackFrames;
        this.condition = condition;
        this.conditionResolver = conditionResolver;
        this.breakpointer = bp;
        interceptors = new ExecutionInterceptors(ImmutableList.of(condition, breakpointer));
        this.otherInterceptors = otherInterceptors;
        this.operations = operations;
        interceptors.addAll(otherInterceptors);
        this.listeners = listeners;
    }

    @Override
    public CobolInterpreter scope(FlowNode scope) {
        return new SmojolInterpreter(runtimeStackFrames.add(scope), condition, conditionResolver, breakpointer, otherInterceptors, listeners, operations);
    }

    @Override
    public CobolVmSignal execute(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify(coloured("Executing " + node.getClass().getSimpleName() + node.label(), 25), node, nodeService);
            return CobolVmSignal.CONTINUE;
        }, new ExecutionContext(node, runtimeStackFrames, nodeService));
    }

    @Override
    public void enter(FlowNode node, FlowNodeService nodeService) {
        condition.evaluate(node);
        String enteringMessage = "Entering " + node.getClass().getSimpleName() + "/" + formatted(node.label());
        condition.run(() -> listeners.notify(coloured(enteringMessage, 240), node, nodeService));
        listeners.notify(coloured(enteringMessage, 240), node, nodeService);
    }

    @Override
    public void exit(FlowNode node, FlowNodeService nodeService) {
        listeners.notify(coloured("Exiting " + node.getClass().getSimpleName() + "/" + formatted(node.label()), 240), node, nodeService);
    }

    @Override
    public CobolVmSignal executeIf(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            LOGGER.finer("Executing an IF condition");
            IfFlowNode ifNode = (IfFlowNode) node;
            boolean trueOrFalse = conditionResolver.resolveIf(node, nodeService);
            if (trueOrFalse) {
                listeners.notify(green(ifNode.getCondition().getText() + " was resolved to TRUE"), node, nodeService);
                LOGGER.finer("ROUTING TO IF-THEN");
                FlowNode ifThenBlock = ifNode.getIfThenBlock();
                return ifThenBlock.acceptInterpreter(this, FlowControl::CONTINUE);
            } else if (ifNode.getIfElseBlock() != null) {
                listeners.notify(green(ifNode.getCondition().getText() + " was resolved to FALSE"), node, nodeService);
                LOGGER.finer("ROUTING TO IF-ELSE");
                FlowNode ifElseBlock = ifNode.getIfElseBlock();
                return ifElseBlock.acceptInterpreter(this, FlowControl::CONTINUE);
            }
            listeners.notify(green(ifNode.getCondition().getText() + " was resolved to FALSE, but no ELSE clause was found"), node, nodeService);
            LOGGER.finer("IF-ELSE BLOCK NOT PRESENT, TERMINATING IF STATEMENT...");
            return CobolVmSignal.CONTINUE;
        }, new ExecutionContext(node, runtimeStackFrames, nodeService));
    }

    @Override
    public CobolVmSignal executePerformProcedure(FlowNode node, List<FlowNode> procedures, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            for (FlowNode procedure : procedures) {
                listeners.notify(cyan("Executing a PERFORM statement: " + procedures.getFirst()), node, nodeService);
                CobolVmSignal signal = procedure.acceptInterpreter(this, FlowControl::STOP);
                listeners.notify(cyan("Returned from PERFORM statement: " + procedures.getFirst()), node, nodeService);

                // Propagate termination because this is the end of the program
                if (signal == CobolVmSignal.TERMINATE) return signal;
            }

            // If a PERFORM has returned (early or normal termination), do not propagate termination any higher
            return CobolVmSignal.CONTINUE;
        }, new ExecutionContext(node, runtimeStackFrames, nodeService));
    }

    @Override
    public CobolVmSignal executeGoto(FlowNode node, List<FlowNode> destinationNodes, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify(red("Executing a GOTO statement: " + destinationNodes.getFirst()), node, nodeService);
            FlowNode destination = destinationNodes.getFirst();
            FlowNode continuationNode = actualDestination(destination);
            CobolVmSignal signal = continuationNode.acceptInterpreter(locator(destination), FlowControl::CONTINUE);
            listeners.notify(red("Exit program in progress, unrolling GO TO..."), node, nodeService);
//            listeners.notifyTermination();
//            System.exit(0);
            return CobolVmSignal.TERMINATE;
        }, new ExecutionContext(node, runtimeStackFrames, nodeService));
    }

    @Override
    public CobolVmSignal executeExit(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            LOGGER.finer(red("Processing EXIT"));
            CobolVmSignal signal = runtimeStackFrames.callSite();
            listeners.notify("EXIT instruction is " + coloured(signal.name(), 207), node, nodeService);
            return signal;
        }, new ExecutionContext(node, runtimeStackFrames, nodeService));
    }

    @Override
    public CobolVmSignal executeNextSentence(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify(purple("Processing NEXT SENTENCE"), node, nodeService);
            return CobolVmSignal.NEXT_SENTENCE;
        }, new ExecutionContext(node, runtimeStackFrames, nodeService));
    }

    @Override
    public CobolVmSignal executeDisplay(FlowNode node, List<CobolParser.DisplayOperandContext> messages, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            messages.forEach(m -> listeners.notify(coloured("CONSOLE >> " + m.getText(), 154), node, nodeService));
            return CobolVmSignal.CONTINUE;
        }, new ExecutionContext(node, runtimeStackFrames, nodeService));
    }

    @Override
    public CobolVmSignal executeMove(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify("Moving " + node, node, nodeService);
            MoveFlowNode move = (MoveFlowNode) node;
            move.getTos().forEach(to -> listeners.notify(coloured(String.format("%s was affected by %s", dataDescription(to, nodeService.getDataStructures()), move.getFromExpression()), 227), node, nodeService));
            operations.move().apply(node).run(runtimeStackFrames.currentData());
            return CobolVmSignal.CONTINUE;
        }, new ExecutionContext(node, runtimeStackFrames, nodeService));
    }

    @Override
    public CobolVmSignal executeAdd(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify("Adding " + node, node, nodeService);
            AddFlowNode add = (AddFlowNode) node;
            add.getTos().forEach(to -> listeners.notify(purple(coloured(String.format("%s was affected by %s", dataDescription(to.generalIdentifier(), nodeService.getDataStructures()), delimited(add.getFroms())), 227)), node, nodeService));
            operations.add().apply(node).run(runtimeStackFrames.currentData());
            return CobolVmSignal.CONTINUE;
        }, new ExecutionContext(node, runtimeStackFrames, nodeService));
    }

    @Override
    public CobolVmSignal executeSubtract(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify("Subtracting " + node, node, nodeService);
            SubtractFlowNode subtract = (SubtractFlowNode) node;
            String lhses = delimited(subtract.getMinuends());
            String rhses = delimited(subtract.getSubtrahends());
            listeners.notify(purple(coloured(String.format("%s was affected by %s", lhses, rhses), 227)), node, nodeService);
            operations.subtract().apply(node).run(runtimeStackFrames.currentData());
            return CobolVmSignal.CONTINUE;
        }, new ExecutionContext(node, runtimeStackFrames, nodeService));
    }

    @Override
    public CobolVmSignal executeMultiply(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify("Adding " + node, node, nodeService);
            MultiplyFlowNode multiply = (MultiplyFlowNode) node;
            listeners.notify(purple(coloured(String.format("%s was affected by %s", multiply.getLhs(), delimited(multiply.getRhses())), 227)), node, nodeService);
            operations.multiply().apply(node).run(runtimeStackFrames.currentData());
            return CobolVmSignal.CONTINUE;
        }, new ExecutionContext(node, runtimeStackFrames, nodeService));
    }

    @Override
    public CobolVmSignal executeDivide(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify("Adding " + node, node, nodeService);
            DivideFlowNode divide = (DivideFlowNode) node;
            listeners.notify(purple(coloured(String.format("%s was affected by %s", delimitedExpressions(divide.getDividendExpressions()), divide.getIntoDivisor()), 227)), node, nodeService);
            operations.divide().apply(node).run(runtimeStackFrames.currentData());
            return CobolVmSignal.CONTINUE;
        }, new ExecutionContext(node, runtimeStackFrames, nodeService));
    }

    @Override
    public CobolVmSignal executeSearch(FlowNode atEndBlock, List<FlowNode> whenPhrases, FlowNodeService nodeService, FlowNode node) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            SearchFlowNode searchNode = (SearchFlowNode) node;
            listeners.notify("Executing SEARCH statement: " + searchNode.getSearchTerm().getText(), node, nodeService);
            boolean shouldExecuteAtEnd = true;
            for (FlowNode whenPhrase : whenPhrases) {
                boolean shouldExecuteWhen = conditionResolver.resolveWhen(whenPhrase, nodeService);
                if (!shouldExecuteWhen) continue;
                shouldExecuteAtEnd = false;
                listeners.notify("Executing WHEN clause: " + formatted(whenPhrase.label()), node, nodeService);
                CobolVmSignal whenSignal = whenPhrase.acceptInterpreter(this, FlowControl::CONTINUE);
                if (whenSignal != CobolVmSignal.CONTINUE) return whenSignal;
            }
            ;
            if (shouldExecuteAtEnd) {
                listeners.notify("Executing AT END clause...", node, nodeService);
                return atEndBlock.acceptInterpreter(this, FlowControl::CONTINUE);
            }
            return CobolVmSignal.CONTINUE;
        }, new ExecutionContext(node, runtimeStackFrames, nodeService));
    }

    @Override
    public CobolVmSignal executeOnClause(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            GenericOnClauseFlowNode onClauseNode = (GenericOnClauseFlowNode) node;
            listeners.notify("Executing ON clause " + formatted(onClauseNode.getCondition().originalText()), node, nodeService);
            boolean trueOrFalse = conditionResolver.resolveOn(onClauseNode, nodeService);
            if (!trueOrFalse) return CobolVmSignal.CONTINUE;
            return onClauseNode.getOnClauseBlock().acceptInterpreterForCompositeExecution(this, FlowControl::CONTINUE);
        }, new ExecutionContext(node, runtimeStackFrames, nodeService));
    }

    @Override
    public void signalTermination() {
        listeners.visitTermination();
        listeners.notifyTermination();
    }

    @Override
    public CobolVmSignal executeCompute(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify("Computing " + node, node, nodeService);
            ComputeFlowNode compute = (ComputeFlowNode) node;
            operations.compute().apply(node).run(runtimeStackFrames.currentData());
            return CobolVmSignal.CONTINUE;
        }, new ExecutionContext(node, runtimeStackFrames, nodeService));
    }

    private String delimited(List<? extends RuleContext> from) {
        return String.join(" , ", from.stream().map(RuleContext::getText).toList());
    }

    private String delimitedExpressions(List<CobolExpression> expressions) {
        return String.join(" , ", expressions.stream().map(Object::toString).toList());
    }


    private String dataDescription(CobolParser.GeneralIdentifierContext identifier, CobolDataStructure dataStructures) {
        List<? extends CobolDataStructure> path = dataStructures.rootRecord(identifier);
        if (path.isEmpty()) return "[NOT FOUND] " + identifier.getText();
        return String.join(" > ", path.stream().map(CobolDataStructure::toString).toList());
    }

    private CobolInterpreter locator(FlowNode specificLocation) {
        return CobolInterpreterFactory.interpreter(new ExecuteAtTargetFlipCondition(specificLocation), conditionResolver, otherInterceptors, listeners, breakpointer, runtimeStackFrames, operations);
    }

    private FlowNode actualDestination(FlowNode destination) {
        if (destination.getClass() == SectionFlowNode.class) return destination;
        ParagraphFlowNode paragraph = (ParagraphFlowNode) destination;
        return paragraph.parentOrSelf();
    }
}
