package org.smojol.common.flowchart;

import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.vm.structure.DataStructureBuilder;

public interface PocOps {
    CobolTreeVisualiser getVisualiser();
    FlowchartBuilderFactoryMethod getFlowchartBuilderFactory();
    EntityNavigatorBuilder getCobolEntityNavigatorBuilder();
    DataStructureBuilder getDataStructureBuilder(CobolEntityNavigator navigator);
}
