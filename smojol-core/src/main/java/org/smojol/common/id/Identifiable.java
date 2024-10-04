package org.smojol.common.id;

import org.apache.commons.lang3.tuple.Pair;

public interface Identifiable {
    String id();
    String label();

    static Identifiable identifiable(Identifiable identifiable) {
        return identifiable;
    }

    static <X, Y> Pair<X, Y> asPair(Pair<X, Y> pair) {
        return pair;
    }
}
