package org.smojol.common.vm.structure;

import java.util.List;

public class IndexProvider {
    private final List<Integer> indices;
    private int counter;

    public IndexProvider(List<Integer> indices) {
        this.counter = 0;
        this.indices = indices;
    }

    public int next() {
        Integer nextIndex = indices.get(counter);
        counter++;
        return nextIndex;
    }

}
