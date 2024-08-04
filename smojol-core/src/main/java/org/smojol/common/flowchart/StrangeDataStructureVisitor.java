package org.smojol.common.flowchart;

import org.smojol.common.vm.structure.CobolDataStructure;

public interface StrangeDataStructureVisitor {
    StrangeDataStructureVisitor visit(CobolDataStructure data);
}
