package com.mojo.algorithms.navigation;


import com.mojo.algorithms.transpiler.LabelledTranspilerCodeBlockNode;
import com.mojo.algorithms.transpiler.TranspilerCodeBlockNode;
import com.mojo.algorithms.domain.TranspilerNode;
import com.mojo.algorithms.transpiler.TreeNodeLocation;

import java.util.Map;

public class TreeNodeParentMapper {
    private final Map<TranspilerNode, TranspilerNode> childToParentMap;

    public TreeNodeParentMapper(TranspilerNode root) {
        ParentBuilderVisitor visitor = new ParentBuilderVisitor();
        new TreeTraversal<TranspilerNode>().run(root, visitor);
        childToParentMap = visitor.getChildToParentMap();
    }

    public TranspilerNode parentOf(TranspilerNode node) {
        return childToParentMap.get(node);
    }

    public TreeNodeLocation parentGraftLocation(TranspilerNode node) {
        TranspilerNode immediateParentScope = childToParentMap.get(node);
        TranspilerNode currentScope = node;
        while (!canGraftOnto(immediateParentScope)) {
            currentScope = immediateParentScope;
            immediateParentScope = childToParentMap.get(currentScope);
        }

        return new TreeNodeLocation(immediateParentScope, currentScope);
    }

    private static boolean canGraftOnto(TranspilerNode parentScope) {
        return parentScope instanceof TranspilerCodeBlockNode || parentScope instanceof LabelledTranspilerCodeBlockNode;
    }

    public void update(TranspilerNode scopeToRecalculateParentsFor) {
        ParentBuilderVisitor visitor = new ParentBuilderVisitor(childToParentMap.get(scopeToRecalculateParentsFor), childToParentMap);
        new TreeTraversal<TranspilerNode>().run(scopeToRecalculateParentsFor, visitor);
    }
}
