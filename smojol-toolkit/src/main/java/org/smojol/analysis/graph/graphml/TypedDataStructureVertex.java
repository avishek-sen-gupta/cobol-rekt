package org.smojol.analysis.graph.graphml;

import lombok.Getter;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.Objects;

@Getter
public class TypedDataStructureVertex implements TypedGraphVertex {
    private final CobolDataStructure node;

    public TypedDataStructureVertex(CobolDataStructure dataNode) {
        this.node = dataNode;
    }

    public String id() {
        return node.getId();
    }

    public String type() {
        return node.getDataType().name();
    }

    public String label() {
        return node.name();
    }

    @Override
    public String name() {
        return node.name();
    }

    public String text() {
        return node.content();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TypedDataStructureVertex that = (TypedDataStructureVertex) o;
        return Objects.equals(node, that.node);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(node);
    }
}
