package org.smojol.analysis.pipeline;

import lombok.Getter;
import org.smojol.common.flowchart.StrangeDataStructureVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;

public class DataStructureExporter implements StrangeDataStructureVisitor {
    @Getter private final SerialisableCobolDataStructure root;

    public DataStructureExporter(SerialisableCobolDataStructure root) {
        this.root = root;
    }

    @Override
    public StrangeDataStructureVisitor visit(CobolDataStructure data) {
        SerialisableCobolDataStructure child = new SerialisableCobolDataStructure(data);
        this.root.add(child);
        return new DataStructureExporter(child);
    }
}
