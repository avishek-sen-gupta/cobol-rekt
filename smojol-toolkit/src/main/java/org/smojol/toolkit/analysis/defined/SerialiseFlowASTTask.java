package org.smojol.toolkit.analysis.defined;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.SerialisableASTFlowNode;
import org.smojol.common.ast.SerialisableFlowNodeASTVisitor;
import org.smojol.common.ast.FlowNodeASTTraversal;

public class SerialiseFlowASTTask {
    public SerialisableASTFlowNode serialisedFlowAST(FlowNode astRoot) {
        SerialisableFlowNodeASTVisitor visitor = new SerialisableFlowNodeASTVisitor(new SerialisableASTFlowNode());
        new FlowNodeASTTraversal<SerialisableASTFlowNode>().accept(astRoot, visitor);
        return visitor.root().getChildren().getFirst();
    }
}
