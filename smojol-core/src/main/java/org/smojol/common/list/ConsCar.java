package org.smojol.common.list;

import com.google.common.collect.ImmutableList;

import java.util.List;
import java.util.Optional;

public class ConsCar {
    public static <T> List<T> tail(List<T> things) {
        if (things.size() <= 1) return ImmutableList.of();
        return things.subList(1, things.size());
    }

    public static <T> Optional<T> head(List<T> things) {
        if (things.isEmpty()) return Optional.empty();
        return Optional.of(things.getFirst());
    }
}
