package org.smojol.common.navigation;

import lombok.Getter;
import org.smojol.common.ast.NodeVisitor;
import org.smojol.common.transpiler.TranspilerNode;

import java.util.HashMap;
import java.util.Map;

public class ParentBuilderVisitor implements NodeVisitor<TranspilerNode> {
    private final TranspilerNode parent;
    @Getter private final Map<TranspilerNode, TranspilerNode> childToParentMap;

    public ParentBuilderVisitor() {
        this(null, new HashMap<>());
    }

    public ParentBuilderVisitor(TranspilerNode parent, Map<TranspilerNode, TranspilerNode> childToParentMap) {
        this.parent = parent;
        this.childToParentMap = childToParentMap;
    }

    @Override
    public NodeVisitor<TranspilerNode> scope(TranspilerNode n) {
        return new ParentBuilderVisitor(n, childToParentMap);
    }

    @Override
    public void visit(TranspilerNode n) {
        childToParentMap.put(n, parent);
    }
}
