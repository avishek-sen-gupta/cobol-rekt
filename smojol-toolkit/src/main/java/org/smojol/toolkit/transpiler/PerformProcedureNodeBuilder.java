package org.smojol.toolkit.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.transpiler.ValueOfNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.TypedRecord;
import org.smojol.toolkit.ast.GoToFlowNode;
import org.smojol.toolkit.ast.PerformProcedureFlowNode;

public class PerformProcedureNodeBuilder {
    public static TranspilerNode build(PerformProcedureFlowNode n, CobolDataStructure dataStructures) {
        if (!n.dependsUponFactor()) return new JumpNode(new LocationNode(n.callTargets().getFirst().name()));
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        ValueOfNode factorValue = new ValueOfNode(nodeBuilder.build(n.getDependingFactor()));
        return ladder(n, factorValue, 0);
    }
}
