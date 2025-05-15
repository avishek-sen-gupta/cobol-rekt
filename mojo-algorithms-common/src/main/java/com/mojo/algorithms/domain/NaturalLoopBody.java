package com.mojo.algorithms.domain;

import com.google.common.collect.ImmutableSet;
import com.mojo.algorithms.id.Identifiable;

import java.util.Set;

public record NaturalLoopBody<V extends Identifiable>(Set<V> loopHeaders, Set<V> loopNodes) {
    public NaturalLoopBody(V loopHeader, Set<V> loopNodes) {
        this(ImmutableSet.of(loopHeader), loopNodes);
    }

    public V singleLoopHeader() {
        return loopHeaders.stream().findAny().get();
    }
}
