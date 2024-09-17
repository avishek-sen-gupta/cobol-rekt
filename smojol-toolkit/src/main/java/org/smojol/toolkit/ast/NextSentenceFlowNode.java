package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.*;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;
import java.util.logging.Logger;

public class NextSentenceFlowNode extends CobolFlowNode {
    private static final Logger logger = Logger.getLogger(NextSentenceFlowNode.class.getName());
    private FlowNode destinationSentenceNode;

    public NextSentenceFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildControlFlow() {
        // scope is the actual SentenceChartNode
        FlowNodeCondition isSentence = n -> n.getClass() == SentenceFlowNode.class;
        FlowNode containingSentence = scope.findUpwards(isSentence, null);
        destinationSentenceNode = containingSentence.next(isSentence, containingSentence, true);
        logger.finer("Next sentence is " + destinationSentenceNode);
    }

    @Override
    public void buildOutgoingFlow() {
        super.buildOutgoingFlow();
        // TODO: For the flowchart, this might create an unwanted line to the next node, but this is needed to build all flows.
    }

    @Override
    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {
        super.acceptUnvisited(visitor, level);
        visitor.visitControlTransfer(this, destinationSentenceNode, new VisitContext(level));
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.NEXT_SENTENCE;
    }

    @Override
    public String label() {
        return "Next Sentence";
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        return interpreter.scope(this).executeNextSentence(this, nodeService);
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.CONTROL_FLOW);
    }
}
