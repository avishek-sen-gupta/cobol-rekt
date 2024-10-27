package org.smojol.common;

import io.vavr.collection.List;

import java.util.Objects;

public class ZipperTestNode {
    private final String id;
    private final io.vavr.collection.List<ZipperTestNode> children;

    public ZipperTestNode(String id, io.vavr.collection.List<ZipperTestNode> children) {
        this.id = id;
        this.children = children;
    }

    public List<ZipperTestNode> astChildren() {
        return children;
    }

    public String id() {
        return id;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ZipperTestNode that)) return false;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
