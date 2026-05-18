package org.smojol.common.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;

public interface FlowNodeService {
  FlowNode register(FlowNode flowNode);

  FlowNode node(ParseTree parseTree, FlowNode scope, StackFrames stackFrames);

  FlowNode sectionOrParaWithName(String name);

  CobolEntityNavigator getNavigator();

  CobolDataStructure getDataStructures();

  FlowNode existingNode(ParseTree parseTree);

  String nextID();
}
