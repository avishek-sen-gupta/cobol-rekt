package org.smojol.common.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;

import java.util.List;

public interface FlowNode {
    void buildFlow();
    void buildOutgoingFlow();
    void buildInternalFlow();
    void buildControlFlow();

    void goesTo(FlowNode successor);
    void addIncomingNode(FlowNode flowNode);

    @Deprecated List<FlowNode> getOutgoingNodes();
    List<FlowNode> getIncomingNodes();

    // TODO: The implementations need rewrite. This is currently not elegant.
    FlowNode next(FlowNodeCondition nodeCondition, FlowNode startingNode, boolean isComplete);

    void linkParentToChild(FlowNodeVisitor visitor, int level);
    void accept(FlowNodeVisitor visitor, int level);
    void accept(FlowNodeVisitor visitor, FlowNodeCondition stopCondition, int level);
    void acceptUnvisited(FlowNodeVisitor visitor, int level);

    List<? extends ParseTree> getChildren();

    FlowNode findUpwards(FlowNodeCondition nodeCondition, FlowNode startingNode);
    FlowNode tail();
    List<FlowNode> astChildren();

    DomainDocument getNotes();

    boolean accessesDatabase();
    boolean isMergeable();
    boolean contains(FlowNode node);

    String id();
    String label();
    String name();
    String originalText();
    ParseTree getExecutionContext();
    FlowNodeType type();
    List<FlowNodeCategory> categories();

    FlowNode passthrough();

    // TODO: Might just be data instead of inheritance
    boolean isPassthrough();

    CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl);

    void addComment(CommentBlock cb);

    List<CommentBlock> getCommentBlocks();
}
