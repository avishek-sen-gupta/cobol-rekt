package org.smojol.common.flowchart;

import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.vm.structure.DataStructureBuilder;

public class DummyPoCOps implements PocOps {
    @Override
    public CobolTreeVisualiser getVisualiser() {
        return null;
    }

    @Override
    public FlowchartBuilderFactoryMethod getFlowchartBuilderFactory() {
        return null;
    }

    @Override
    public EntityNavigatorBuilder getCobolEntityNavigatorBuilder() {
        return null;
    }

    @Override
    public DataStructureBuilder getDataStructureBuilder(CobolEntityNavigator navigator) {
        return null;
    }
}
