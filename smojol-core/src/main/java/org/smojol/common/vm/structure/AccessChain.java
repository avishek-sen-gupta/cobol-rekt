package org.smojol.common.vm.structure;

import java.util.ArrayList;
import java.util.List;

public class AccessChain {
    private final List<AccessLink> links;

    public AccessChain(List<AccessLink> links) {
        this.links = links;
    }

    public AccessChain copy() {
        return new AccessChain(new ArrayList<>(links));
    }

    public AccessChain append(AccessChain childChain) {
        links.addAll(childChain.links);
        return this;
    }

    public AccessChain curriedIndex() {
        links.add(new CurriedIndexedAccess());
        return this;
    }

    public AccessChain staticIndex(int index) {
        links.add(new IndexedAccess(index));
        return this;
    }

    public CobolDataStructure run(List<Integer> indices) {
        IndexProvider indexProvider = new IndexProvider(indices);
        CobolDataStructure current = links.getFirst().run(null, indexProvider);
        for (AccessLink link : links.subList(1, links.size())) {
            current = link.run(current, indexProvider);
        }
        return current;
    }
}
