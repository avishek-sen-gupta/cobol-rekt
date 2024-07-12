package org.smojol.analysis.visualisation;

import lombok.Getter;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.vm.structure.Format1DataStructureBuilder;
import org.smojol.interpreter.structure.CobolDataStructureBuilder;
import org.smojol.common.vm.strategy.UnresolvedReferenceStrategy;
import org.smojol.common.flowchart.CobolTreeVisualiser;
import org.smojol.common.flowchart.FlowchartBuilderFactoryMethod;
import org.smojol.common.vm.structure.DataStructureBuilder;

public class ComponentsBuilder {
    @Getter private final CobolTreeVisualiser visualiser;
    @Getter private final FlowchartBuilderFactoryMethod flowchartBuilderFactory;
    private final EntityNavigatorBuilder cobolEntityNavigatorBuilder;
    private final UnresolvedReferenceStrategy unresolvedReferenceStrategy;
    private final Format1DataStructureBuilder format1DataStructureBuilder;

    public ComponentsBuilder(CobolTreeVisualiser visualiser, FlowchartBuilderFactoryMethod flowchartBuilderFactory, EntityNavigatorBuilder cobolEntityNavigatorBuilder, UnresolvedReferenceStrategy unresolvedReferenceStrategy, Format1DataStructureBuilder format1DataStructureBuilder) {
        this.visualiser = visualiser;
        this.flowchartBuilderFactory = flowchartBuilderFactory;
        this.cobolEntityNavigatorBuilder = cobolEntityNavigatorBuilder;
        this.unresolvedReferenceStrategy = unresolvedReferenceStrategy;
        this.format1DataStructureBuilder = format1DataStructureBuilder;
    }

    public EntityNavigatorBuilder getCobolEntityNavigatorBuilder() {
        return cobolEntityNavigatorBuilder;
    }

    public DataStructureBuilder getDataStructureBuilder(CobolEntityNavigator navigator) {
        return new CobolDataStructureBuilder(navigator, unresolvedReferenceStrategy, format1DataStructureBuilder);
    }
}
