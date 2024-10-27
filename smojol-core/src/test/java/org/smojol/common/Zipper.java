package org.smojol.common;

import io.vavr.collection.List;
import lombok.Getter;
import org.smojol.common.navigation.GenericTreeNode;

@Getter
public class Zipper<T extends GenericTreeNode<T>> {
    private final List<T> thread;
    private final T current;

    public Zipper(List<T> thread, T current) {
        this.thread = thread;
        this.current = current;
    }

    public Zipper<T> down(T child) {
        int childIndex = current.astChildren().indexOf(child);
        if (childIndex == -1) return this;
        T newCurrent = current.astChildren().get(childIndex);
        return new Zipper<>(thread.prepend(current), newCurrent);
    }
}
