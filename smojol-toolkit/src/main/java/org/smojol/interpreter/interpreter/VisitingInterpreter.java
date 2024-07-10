package org.smojol.interpreter.interpreter;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.ast.*;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;
import org.smojol.common.vm.interpreter.*;
import org.smojol.common.vm.stack.ExecutionContext;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;

import static org.smojol.common.flowchart.ConsoleColors.*;
import static org.smojol.common.flowchart.NodeText.formatted;

public class VisitingInterpreter implements CobolInterpreter {
    private final ExecutionInterceptors interceptors;
    private final ExecutionListener listeners;
    private final CobolInterpreter interpreter;

    public VisitingInterpreter(CobolInterpreter interpreter, List<ExecutionInterceptor> interceptors, ExecutionListener listeners) {
        this.interpreter = interpreter;
        this.listeners = listeners;
        this.interceptors = new ExecutionInterceptors(interceptors);
    }

    @Override
    public CobolInterpreter scope(FlowNode scope) {
        return interpreter.scope(scope);
    }

    @Override
    public CobolVmSignal execute(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify(coloured("Executing " + node.getClass().getSimpleName() + node.label(), 25), node, nodeService);
            return interpreter.execute(node, nodeService);
        }, new ExecutionContext(node, interpreter.getStackFrames(), nodeService));
    }

    @Override
    public void enter(FlowNode node, FlowNodeService nodeService) {
        interpreter.enter(node, nodeService);
    }

    @Override
    public void exit(FlowNode node, FlowNodeService nodeService) {
        listeners.notify(coloured("Exiting " + node.getClass().getSimpleName() + "/" + formatted(node.label()), 240), node, nodeService);
    }

    @Override
    public CobolVmSignal executeIf(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            System.out.println("Executing an IF condition");
            return interpreter.executeIf(node, nodeService);
        }, new ExecutionContext(node, interpreter.getStackFrames(), nodeService));
    }

    @Override
    public CobolVmSignal executePerformProcedure(FlowNode node, List<FlowNode> procedures, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            return interpreter.executePerformProcedure(node, procedures, nodeService);
        }, new ExecutionContext(node, interpreter.getStackFrames(), nodeService));
    }

    @Override
    public CobolVmSignal executeGoto(FlowNode node, List<FlowNode> destinationNodes, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify(red("Executing a GOTO statement: " + destinationNodes.getFirst()), node, nodeService);
            return interpreter.executeGoto(node, destinationNodes, nodeService);
        }, new ExecutionContext(node, interpreter.getStackFrames(), nodeService));
    }

    @Override
    public CobolVmSignal executeExit(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            System.out.println(red("Processing EXIT"));
            return interpreter.executeExit(node, nodeService);
        }, new ExecutionContext(node, interpreter.getStackFrames(), nodeService));
    }

    @Override
    public CobolVmSignal executeNextSentence(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify(purple("Processing NEXT SENTENCE"), node, nodeService);
            return interpreter.executeNextSentence(node, nodeService);
        }, new ExecutionContext(node, interpreter.getStackFrames(), nodeService));
    }

    @Override
    public CobolVmSignal executeDisplay(FlowNode node, List<CobolParser.DisplayOperandContext> messages, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            messages.forEach(m -> listeners.notify(coloured("CONSOLE >> " + m.getText(), 154), node, nodeService));
            return interpreter.executeDisplay(node, messages, nodeService);
        }, new ExecutionContext(node, interpreter.getStackFrames(), nodeService));
    }

    @Override
    public CobolVmSignal executeMove(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify("Moving " + node, node, nodeService);
            return interpreter.executeMove(node, nodeService);
        }, new ExecutionContext(node, interpreter.getStackFrames(), nodeService));
    }

    @Override
    public CobolVmSignal executeAdd(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify("Adding " + node, node, nodeService);
            return interpreter.executeAdd(node, nodeService);
        }, new ExecutionContext(node, interpreter.getStackFrames(), nodeService));
    }

    @Override
    public CobolVmSignal executeSubtract(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify("Subtracting " + node, node, nodeService);
            return interpreter.executeSubtract(node, nodeService);
        }, new ExecutionContext(node, interpreter.getStackFrames(), nodeService));
    }

    @Override
    public CobolVmSignal executeMultiply(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify("Adding " + node, node, nodeService);
            return interpreter.executeMultiply(node, nodeService);
        }, new ExecutionContext(node, interpreter.getStackFrames(), nodeService));
    }

    @Override
    public CobolVmSignal executeDivide(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify("Adding " + node, node, nodeService);
            return interpreter.executeDivide(node, nodeService);
        }, new ExecutionContext(node, interpreter.getStackFrames(), nodeService));
    }

    @Override
    public CobolVmSignal executeSearch(FlowNode atEndBlock, List<FlowNode> whenPhrases, FlowNodeService nodeService, FlowNode node) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            SearchFlowNode searchNode = (SearchFlowNode) node;
            listeners.notify("Executing SEARCH statement: " + searchNode.getSearchTerm().getText(), node, nodeService);
            return interpreter.executeSearch(atEndBlock, whenPhrases, nodeService, searchNode);
        }, new ExecutionContext(node, interpreter.getStackFrames(), nodeService));
    }

    @Override
    public CobolVmSignal executeOnClause(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            GenericOnClauseFlowNode onClauseNode = (GenericOnClauseFlowNode) node;
            listeners.notify("Executing ON clause " + formatted(onClauseNode.getCondition().originalText()), node, nodeService);
            return interpreter.executeOnClause(node, nodeService);
        }, new ExecutionContext(node, interpreter.getStackFrames(), nodeService));
    }

    @Override
    public void signalTermination() {
        listeners.visitTermination();
        listeners.notifyTermination();
        interpreter.signalTermination();
    }

    @Override
    public StackFrames getStackFrames() {
        return interpreter.getStackFrames();
    }

    @Override
    public CobolVmSignal executeCompute(FlowNode node, FlowNodeService nodeService) {
        return interceptors.run(() -> {
            listeners.visit(node, nodeService);
            listeners.notify("Computing " + node, node, nodeService);
            return interpreter.executeCompute(node, nodeService);
        }, new ExecutionContext(node, interpreter.getStackFrames(), nodeService));
    }
}
