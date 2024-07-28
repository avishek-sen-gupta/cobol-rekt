package org.smojol.analysis.pipeline;

import org.smojol.common.flowchart.*;
import org.smojol.interpreter.navigation.FlowNodeASTTraversal;

public class SerialiseFlowASTTask {
    public SerialisableASTFlowNode serialisedFlowAST(FlowNode astRoot) {
        SerialisableFlowNodeASTVisitor visitor = new SerialisableFlowNodeASTVisitor();
        new FlowNodeASTTraversal<SerialisableASTFlowNode>().build(astRoot, visitor);
        return visitor.root();
    }
}
