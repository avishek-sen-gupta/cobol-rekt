package org.smojol.common.ast;

import org.smojol.common.vm.structure.CobolDataStructure;

public interface CobolDataStructureCondition {
    boolean apply(CobolDataStructure dataStructure);
}
