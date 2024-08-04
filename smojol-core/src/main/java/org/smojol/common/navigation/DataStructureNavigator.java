package org.smojol.common.navigation;

import org.smojol.common.ast.CobolDataStructureCondition;
import org.smojol.common.vm.structure.CobolDataStructure;

public class DataStructureNavigator {
    private final CobolDataStructure root;

    public DataStructureNavigator(CobolDataStructure root) {
        this.root = root;
    }

    public CobolDataStructure findByCondition(CobolDataStructureCondition c) {
        return searchRecursively(root, c);
    }

    private CobolDataStructure searchRecursively(CobolDataStructure current, CobolDataStructureCondition c) {
        if (c.apply(current)) return current;
        for (CobolDataStructure child : current.subStructures()) {
            CobolDataStructure found = searchRecursively(child, c);
            if (found != null) return found;
        }
        return null;
    }
}
