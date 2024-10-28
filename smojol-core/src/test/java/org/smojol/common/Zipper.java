package org.smojol.common;

import io.vavr.collection.List;
import lombok.Getter;
import org.smojol.common.list.CarCdr;
import org.smojol.common.navigation.GenericTreeNode;

import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Predicate;

public class Zipper<T extends GenericTreeNode<T>> {
    @Getter private final List<T> thread;
    @Getter private final T current;
    private final BiFunction<T, java.util.List<T>, T> cloneCtor;

    public Zipper(List<T> thread, T current, BiFunction<T, java.util.List<T>, T> cloneCtor) {
        this.thread = thread;
        this.current = current;
        this.cloneCtor = cloneCtor;
    }

    public Zipper<T> down(T child) {
        int childIndex = current.astChildren().indexOf(child);
        if (childIndex == -1) return this;
        return new Zipper<>(thread.prepend(current), child, cloneCtor);
    }

    public Zipper<T> down(Predicate<T> condition) {
        java.util.List<T> filtered = current.astChildren().stream().filter(condition).toList();
        Optional<T> head = CarCdr.head(filtered);
        if (head.isEmpty()) return this;
        return down(head.get());
    }

    public Zipper<T> up() {
        if (thread.isEmpty()) return this;
        return new Zipper<>(thread.tail(), thread.head(), cloneCtor);
    }

    public Zipper<T> replaceChildren(List<T> newChildren) {
        return up().replaceChild(current, cloneCtor.apply(current, newChildren.asJava()));
    }

    private Zipper<T> replaceChild(T replacedChild, T replacingChild) {
        List<T> currentChildren = List.ofAll(current.astChildren());
        List<T> updatedChildren = currentChildren.update(currentChildren.indexOf(replacedChild), replacingChild);
        Zipper<T> upZipper = up();
        T nodeCopy = cloneCtor.apply(current, updatedChildren.asJava());
        if (upZipper == this) return new Zipper<>(List.of(), nodeCopy, cloneCtor);
        return upZipper.replaceChild(current, nodeCopy);
    }
}
