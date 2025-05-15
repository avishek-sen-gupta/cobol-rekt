package com.mojo.algorithms.id;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

public interface Identifiable {
    static <X, Y> Pair<X, Y> pair(X left, Y right) {
        return ImmutablePair.of(left, right);
    }

    String id();
    String label();

    static Identifiable identifiable(Identifiable identifiable) {
        return identifiable;
    }

    static <X, Y> Pair<X, Y> asPair(Pair<X, Y> pair) {
        return pair;
    }
}
