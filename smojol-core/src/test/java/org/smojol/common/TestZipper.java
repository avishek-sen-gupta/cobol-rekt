package org.smojol.common;

import io.vavr.collection.List;
import lombok.Getter;

import java.util.function.Predicate;

@Getter
public class TestZipper {
    private final List<ZipperTestNode> thread;
    private final ZipperTestNode current;

    public TestZipper(List<ZipperTestNode> thread, ZipperTestNode current) {
        this.thread = thread;
        this.current = current;
    }

    public TestZipper down(ZipperTestNode child) {
        int childIndex = current.astChildren().indexOf(child);
        if (childIndex == -1) return this;
        return new TestZipper(thread.prepend(current), child);
    }

    public TestZipper down(Predicate<ZipperTestNode> condition) {
        List<ZipperTestNode> filtered = current.astChildren().filter(condition);
        if (filtered.isEmpty()) return this;
        return down(filtered.head());
    }

    public TestZipper up() {
        if (thread.isEmpty()) return this;
        return new TestZipper(thread.tail(), thread.head());
    }

    public TestZipper replaceChildren(List<ZipperTestNode> newChildren) {
        return up().replaceChild(current, new ZipperTestNode(current.id(), newChildren));
    }

    private TestZipper replaceChild(ZipperTestNode replacedChild, ZipperTestNode replacingChild) {
        List<ZipperTestNode> currentChildren = current.astChildren();
        List<ZipperTestNode> updatedChildren = currentChildren.update(currentChildren.indexOf(replacedChild), replacingChild);
        TestZipper upZipper = up();
        ZipperTestNode nodeCopy = new ZipperTestNode(current.id(), updatedChildren);
        if (upZipper == this) return new TestZipper(List.of(), nodeCopy);
        return upZipper.replaceChild(current, nodeCopy);
    }
}
