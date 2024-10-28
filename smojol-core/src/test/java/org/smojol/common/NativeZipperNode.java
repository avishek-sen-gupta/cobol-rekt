package org.smojol.common;

import io.vavr.collection.List;

import java.util.Objects;

public class NativeZipperNode {
    private final String id;
    private final io.vavr.collection.List<NativeZipperNode> children;

    public NativeZipperNode(String id, io.vavr.collection.List<NativeZipperNode> children) {
        this.id = id;
        this.children = children;
    }

    public List<NativeZipperNode> astChildren() {
        return children;
    }

    public String id() {
        return id;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof NativeZipperNode that)) return false;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
