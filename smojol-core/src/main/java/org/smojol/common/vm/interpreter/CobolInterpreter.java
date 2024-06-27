package org.smojol.common.vm.interpreter;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;

import java.util.List;

public interface CobolInterpreter {
    CobolInterpreter scope(FlowNode scope);
    void enter(FlowNode node, FlowNodeService nodeService);
    void exit(FlowNode node, FlowNodeService nodeService);
    CobolVmSignal execute(FlowNode node, FlowNodeService nodeService);
    CobolVmSignal executeIf(FlowNode node, FlowNodeService nodeService);
    CobolVmSignal executePerformProcedure(FlowNode node, List<FlowNode> procedures, FlowNodeService nodeService);
    CobolVmSignal executeGoto(FlowNode node, List<FlowNode> destinationNodes, FlowNodeService nodeService);
    CobolVmSignal executeExit(FlowNode node, FlowNodeService nodeService);
    CobolVmSignal executeNextSentence(FlowNode node, FlowNodeService nodeService);
    CobolVmSignal executeDisplay(FlowNode node, List<CobolParser.DisplayOperandContext> messages, FlowNodeService nodeService);
    CobolVmSignal executeMove(FlowNode node, FlowNodeService nodeService);
    CobolVmSignal executeAdd(FlowNode node, FlowNodeService nodeService);
    CobolVmSignal executeSubtract(FlowNode node, FlowNodeService nodeService);
    CobolVmSignal executeMultiply(FlowNode node, FlowNodeService nodeService);
    CobolVmSignal executeDivide(FlowNode node, FlowNodeService nodeService);
    CobolVmSignal executeCompute(FlowNode node, FlowNodeService nodeService);
    CobolVmSignal executeSearch(FlowNode node, List<FlowNode> whenPhrases, FlowNodeService nodeService, FlowNode searchFlowNode);
    CobolVmSignal executeOnClause(FlowNode node, FlowNodeService nodeService);
}
