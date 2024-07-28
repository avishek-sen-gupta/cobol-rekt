package org.smojol.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.*;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;
import java.util.stream.Collectors;

import static guru.nidi.graphviz.model.Factory.mutNode;

public class GoToFlowNode extends CobolFlowNode {

    private List<FlowNode> destinationNodes;

    public GoToFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildOutgoingFlow() {
        super.buildOutgoingFlow();
    }

    @Override
    public String label() {
        return originalText();
    }

    @Override
    public void buildControlFlow() {
        CobolParser.GoToStatementContext goToStatement = new SyntaxIdentity<CobolParser.GoToStatementContext>(getExecutionContext()).get();
        List<CobolParser.ProcedureNameContext> procedureNames = goToStatement.procedureName();
        System.out.println("Found a GO TO, routing to " + procedureNames);
        destinationNodes = procedureNames.stream().map(p -> nodeService.sectionOrParaWithName(p.paragraphName().getText())).collect(Collectors.toList());
    }

    @Override
    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {
        super.acceptUnvisited(visitor, level);
        destinationNodes.forEach(destinationNode -> visitor.visitControlTransfer(this, destinationNode, new VisitContext(level)));
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        return interpreter.scope(this).executeGoto(this, destinationNodes, nodeService);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.CONTROL_FLOW;
    }
}
