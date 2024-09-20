package org.smojol.toolkit.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.id.IdProvider;
import org.smojol.toolkit.interpreter.stack.CobolStackFrames;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.stack.StackFrames;

import java.util.ArrayList;
import java.util.List;

public class FlowNodeServiceImpl implements FlowNodeService {
    int counter = 0;
    private final List<FlowNode> nodes = new ArrayList<>();
    private CobolEntityNavigator navigator;
    private final CobolDataStructure dataStructures;
    private final IdProvider idProvider;

    public FlowNodeServiceImpl(CobolEntityNavigator navigator, CobolDataStructure dataStructures, IdProvider idProvider) {
        this.navigator = navigator;
        this.dataStructures = dataStructures;
        this.idProvider = idProvider;
    }

    public FlowNode register(FlowNode flowNode) {
        int index = nodes.indexOf(flowNode);
        if (index != -1) return nodes.get(index);
        nodes.add(flowNode);
        return flowNode;
    }

    public FlowNode node(ParseTree parseTree, FlowNode scope, StackFrames stackFrames) {
        if (parseTree == null) return new DummyFlowNode(this, stackFrames);
        FlowNode n = CobolFlowNodeFactory.newNode(parseTree, scope, this, stackFrames);
        int index = nodes.indexOf(n);
        if (index != -1) return nodes.get(index);
        nodes.add(n);
        return n;
    }

    public FlowNode unmodifiedNode(ParseTree parseTree, FlowNode scope, StackFrames stackFrames) {
        if (parseTree == null) return new DummyFlowNode(this, stackFrames);
        FlowNode n = CobolFlowNodeFactory.newUnmodifiedNode(parseTree, scope, this, stackFrames);
        int index = nodes.indexOf(n);
        if (index != -1) return nodes.get(index);
        nodes.add(n);
        return n;
    }

    @Override
    public FlowNode sectionOrParaWithName(String name) {
        ParseTree target = navigator.target(name);
        FlowNode existingGroup = existingNode(target);

        // Only place where it's acceptable to have null scope since this section/para is not part of the flowchart's AST, so it's a dummy placeholder
        // Only place where we can initialise a new StackFrame because this is only going to be called during visualisation where stack frame info is not needed
        return existingGroup != null ? existingGroup : node(target, null, new CobolStackFrames());
    }

    @Override
    public CobolEntityNavigator getNavigator() {
        return navigator;
    }
    @Override
    public CobolDataStructure getDataStructures() {
        return dataStructures;
    }

    @Override
    public FlowNode existingNode(ParseTree parseTree) {
        return nodes.stream().filter(n -> n.getExecutionContext() == parseTree).findFirst().orElse(null);
    }

    @Override
    public String nextID() {
        return idProvider.next();
    }
}
