package org.smojol.common.vm.interpreter;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;

public class AlwaysBooleanResolver implements BooleanResolver {
    public static BooleanResolver ALWAYS_TRUE = new AlwaysBooleanResolver(true);
    public static BooleanResolver ALWAYS_FALSE = new AlwaysBooleanResolver(false);
    private final boolean alwaysValue;

    public AlwaysBooleanResolver(boolean alwaysValue) {
        this.alwaysValue = alwaysValue;
    }

    @Override
    public boolean resolve(FlowNode node, CobolParser.ConditionContext condition, FlowNodeService nodeService) {
        return alwaysValue;
    }
}
