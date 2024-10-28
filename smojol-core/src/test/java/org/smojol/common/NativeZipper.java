package org.smojol.common;

import io.vavr.collection.List;
import lombok.Getter;

import java.util.function.Predicate;

@Getter
public class NativeZipper {
    private final List<NativeZipperNode> thread;
    private final NativeZipperNode current;

    public NativeZipper(List<NativeZipperNode> thread, NativeZipperNode current) {
        this.thread = thread;
        this.current = current;
    }

    public NativeZipper down(NativeZipperNode child) {
        int childIndex = current.astChildren().indexOf(child);
        if (childIndex == -1) return this;
        return new NativeZipper(thread.prepend(current), child);
    }

    public NativeZipper down(Predicate<NativeZipperNode> condition) {
        List<NativeZipperNode> filtered = current.astChildren().filter(condition);
        if (filtered.isEmpty()) return this;
        return down(filtered.head());
    }

    public NativeZipper up() {
        if (thread.isEmpty()) return this;
        return new NativeZipper(thread.tail(), thread.head());
    }

    public NativeZipper replaceChildren(List<NativeZipperNode> newChildren) {
        return up().replaceChild(current, new NativeZipperNode(current.id(), newChildren));
    }

    private NativeZipper replaceChild(NativeZipperNode replacedChild, NativeZipperNode replacingChild) {
        List<NativeZipperNode> currentChildren = current.astChildren();
        List<NativeZipperNode> updatedChildren = currentChildren.update(currentChildren.indexOf(replacedChild), replacingChild);
        NativeZipper upZipper = up();
        NativeZipperNode nodeCopy = new NativeZipperNode(current.id(), updatedChildren);
        if (upZipper == this) return new NativeZipper(List.of(), nodeCopy);
        return upZipper.replaceChild(current, nodeCopy);
    }
}
