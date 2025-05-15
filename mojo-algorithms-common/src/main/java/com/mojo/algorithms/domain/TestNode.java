package com.mojo.algorithms.domain;

import com.mojo.algorithms.id.Identifiable;

import java.util.Objects;

public record TestNode(String id) implements Identifiable {

    @Override
    public String label() {
        return id;
    }

    @Override
    public String toString() {
        return id;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof TestNode that)) return false;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
