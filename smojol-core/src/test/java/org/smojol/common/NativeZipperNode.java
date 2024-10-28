package org.smojol.common;

import io.vavr.collection.List;

import java.util.Objects;

public class NativeZipperNode implements ZipperNode<NativeZipperNode> {
    private final String id;
    private final List<NativeZipperNode> children;

    public NativeZipperNode(String id, List<NativeZipperNode> children) {
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

    @Override
    public NativeZipperNode clone(NativeZipperNode original, List<NativeZipperNode> children) {
        return new NativeZipperNode(id, children);
    }

    @Override
    public List<NativeZipperNode> children() {
        return children;
    }
}
