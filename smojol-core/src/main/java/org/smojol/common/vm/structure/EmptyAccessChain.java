package org.smojol.common.vm.structure;

import com.google.common.collect.ImmutableList;

public class EmptyAccessChain extends AccessChain {
    public EmptyAccessChain() {
        super(ImmutableList.of());
    }
}
