package org.smojol.common.vm.structure;

import java.util.List;

public class ListParseTreeChain<T> {
    private final List<T> list;

    public ListParseTreeChain(List<T> list) {
        this.list = list;
    }

    public List<T> get() {
        return list;
    }

    public boolean exists() {
        return !list.isEmpty();
    }
}
