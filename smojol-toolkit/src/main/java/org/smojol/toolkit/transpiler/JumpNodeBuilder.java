package org.smojol.toolkit.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerNode;
import com.mojo.algorithms.domain.TypedRecord;
import com.mojo.algorithms.transpiler.*;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.GoToFlowNode;

public class JumpNodeBuilder {
    public static TranspilerNode build(GoToFlowNode n, CobolDataStructure dataStructures) {
        if (n.callTargets() == null) throw new RuntimeException("Call targets are empty: " + n.originalText());
        if (!n.dependsUponFactor()) return new JumpTranspilerNode(new NamedLocationNode(n.callTargets().getFirst().name()));
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        ValueOfNode factorValue = new ValueOfNode(nodeBuilder.build(n.getDependingFactor()));
        return ladder(n, factorValue, 0);
    }

    private static TranspilerNode ladder(GoToFlowNode n, ValueOfNode factorValue, int index) {
        if (index == n.callTargets().size() - 1) return new IfTranspilerNode(new EqualToNode(factorValue, new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(index + 1))),
                new DetachedTranspilerCodeBlockNode(ImmutableList.of(new JumpTranspilerNode(new NamedLocationNode(n.callTargets().get(index).name())))),
                new DetachedTranspilerCodeBlockNode());
        return new IfTranspilerNode(new EqualToNode(factorValue, new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(index + 1))),
                new DetachedTranspilerCodeBlockNode(ImmutableList.of(new JumpTranspilerNode(new NamedLocationNode(n.callTargets().get(index).name())))),
                new DetachedTranspilerCodeBlockNode(ladder(n, factorValue, index + 1)));
    }
}
