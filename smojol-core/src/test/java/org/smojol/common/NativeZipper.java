package org.smojol.common;

import io.vavr.collection.List;
import lombok.Getter;

import java.util.function.Predicate;

@Getter
public class NativeZipper<T extends ZipperNode<T>> {
    private final List<T> thread;
    private final T current;

    public NativeZipper(List<T> thread, T current) {
        this.thread = thread;
        this.current = current;
    }

    public T current() {
        return current;
    }

    public NativeZipper<T> down(T child) {
        int childIndex = current.children().indexOf(child);
        if (childIndex == -1) return this;
        return new NativeZipper<>(thread.prepend(current), child);
    }

    public NativeZipper<T> down(Predicate<T> condition) {
        List<T> filtered = current.children().filter(condition);
        if (filtered.isEmpty()) return this;
        return down(filtered.head());
    }

    public NativeZipper<T> up() {
        if (thread.isEmpty()) return this;
        return new NativeZipper<>(thread.tail(), thread.head());
    }

    public NativeZipper<T> replaceChildren(List<T> newChildren) {
        return up().replaceChild(current, current.clone(current, newChildren));
    }

    private NativeZipper<T> replaceChild(T replacedChild, T replacingChild) {
        List<T> currentChildren = current.children();
        List<T> updatedChildren = currentChildren.update(currentChildren.indexOf(replacedChild), replacingChild);
        NativeZipper<T> upZipper = up();
        T nodeCopy = current.clone(current, updatedChildren);
        if (upZipper == this) return new NativeZipper<>(List.of(), nodeCopy);
        return upZipper.replaceChild(current, nodeCopy);
    }
}
