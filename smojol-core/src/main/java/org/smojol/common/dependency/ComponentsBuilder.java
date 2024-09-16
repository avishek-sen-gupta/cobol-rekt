package org.smojol.common.dependency;

import lombok.Getter;
import org.smojol.common.id.IdProvider;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.common.structure.CobolDataStructureBuilder;
import org.smojol.common.vm.structure.Format1DataStructureBuilder;
import org.smojol.common.vm.strategy.UnresolvedReferenceStrategy;
import org.smojol.common.ast.CobolTreeVisualiser;
import org.smojol.common.flowchart.FlowchartBuilderFactoryMethod;

public class ComponentsBuilder {
    @Getter private final CobolTreeVisualiser visualiser;
    @Getter private final FlowchartBuilderFactoryMethod flowchartBuilderFactory;
    @Getter private final EntityNavigatorBuilder cobolEntityNavigatorBuilder;
    private final UnresolvedReferenceStrategy unresolvedReferenceStrategy;
    private final Format1DataStructureBuilder format1DataStructureBuilder;
    @Getter private final IdProvider idProvider;
    @Getter private final ResourceOperations resourceOperations;

    public ComponentsBuilder(CobolTreeVisualiser visualiser, FlowchartBuilderFactoryMethod flowchartBuilderFactory, EntityNavigatorBuilder cobolEntityNavigatorBuilder, UnresolvedReferenceStrategy unresolvedReferenceStrategy, Format1DataStructureBuilder format1DataStructureBuilder, IdProvider idProvider, ResourceOperations resourceOperations) {
        this.visualiser = visualiser;
        this.flowchartBuilderFactory = flowchartBuilderFactory;
        this.cobolEntityNavigatorBuilder = cobolEntityNavigatorBuilder;
        this.unresolvedReferenceStrategy = unresolvedReferenceStrategy;
        this.format1DataStructureBuilder = format1DataStructureBuilder;
        this.idProvider = idProvider;
        this.resourceOperations = resourceOperations;
    }

    public CobolDataStructureBuilder getDataStructureBuilder(CobolEntityNavigator navigator) {
        return new CobolDataStructureBuilder(navigator, unresolvedReferenceStrategy, format1DataStructureBuilder, idProvider);
    }
}
