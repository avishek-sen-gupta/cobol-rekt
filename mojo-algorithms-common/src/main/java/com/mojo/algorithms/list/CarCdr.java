package com.mojo.algorithms.list;

import com.google.common.collect.ImmutableList;

import java.util.List;
import java.util.Optional;

public class CarCdr {
    public static <T> List<T> tail(List<T> things) {
        if (things.size() <= 1) return ImmutableList.of();
        return things.subList(1, things.size());
    }

    public static <T> Optional<T> head(List<T> things) {
        if (things.isEmpty()) return Optional.empty();
        return Optional.of(things.getFirst());
    }

    public static <T> List<T> init(List<T> things) {
        if (things.isEmpty() || things.size() == 1) return ImmutableList.of();
        return things.subList(0, things.size() - 1);
    }
}
