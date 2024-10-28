package org.smojol.common;

import io.vavr.collection.List;

public interface ZipperNode<T> {
    T clone(T original, List<T> children);
    List<T> children();
}
