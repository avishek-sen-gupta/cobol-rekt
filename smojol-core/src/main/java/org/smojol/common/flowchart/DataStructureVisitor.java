package org.smojol.common.flowchart;

import org.smojol.common.vm.structure.CobolDataStructure;

public interface DataStructureVisitor {
    CobolDataStructure visit(CobolDataStructure data, CobolDataStructure parent, CobolDataStructure root);
}
