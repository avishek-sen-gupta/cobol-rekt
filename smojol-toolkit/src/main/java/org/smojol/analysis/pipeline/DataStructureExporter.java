package org.smojol.analysis.pipeline;

import lombok.Getter;
import org.smojol.common.vm.structure.ScopedDataStructureVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;

public class DataStructureExporter implements ScopedDataStructureVisitor {
    @Getter private final SerialisableCobolDataStructure root;

    public DataStructureExporter(SerialisableCobolDataStructure root) {
        this.root = root;
    }

    @Override
    public ScopedDataStructureVisitor visit(CobolDataStructure data) {
        SerialisableCobolDataStructure child = new SerialisableCobolDataStructure(data);
        this.root.add(child);
        return new DataStructureExporter(child);
    }
}
