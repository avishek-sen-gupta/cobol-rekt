package org.smojol.common.flowchart;

import org.smojol.common.id.IdProvider;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.structure.CobolDataStructure;

public interface FlowchartBuilderFactoryMethod {
    FlowchartBuilder apply(CobolEntityNavigator navigator, CobolDataStructure dataStructures, IdProvider idProvider);
}
