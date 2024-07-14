package org.smojol.analysis.graph.graphml;

import lombok.Getter;
import org.smojol.common.flowchart.FlowNode;

import java.util.Objects;

@Getter
public class TypedCodeVertex implements TypedGraphVertex {
    private final FlowNode node;

    public TypedCodeVertex(FlowNode node) {
        this.node = node;
    }

    public String id() {
        return node.id();
    }

    public String type() {
        return node.type().name();
    }

    public String label() {
        return node.label();
    }

    @Override
    public String name() {
        return node.name();
    }

    public String text() {
        return node.originalText();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TypedCodeVertex that = (TypedCodeVertex) o;
        return Objects.equals(node, that.node);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(node);
    }
}
