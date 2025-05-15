package com.mojo.algorithms.domain;

import com.mojo.algorithms.transpiler.FlowNodeLike;
import lombok.Getter;

import java.util.Objects;

@Getter
public class TypedCodeVertex implements TypedGraphVertex {
    private final FlowNodeLike node;
    private final String namespace;

    public TypedCodeVertex(FlowNodeLike node, String namespace) {
        this.node = node;
        this.namespace = namespace;
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
    public String namespace() {
        return namespace;
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

    @Override
    public String toString() {
        return type() + " / " + text();
    }
}
