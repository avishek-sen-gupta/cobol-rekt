package org.smojol.common.vm.structure;

import com.google.common.collect.ImmutableList;

import java.util.List;

public class EmptyAccessChain extends AccessChain {
    public EmptyAccessChain() {
        super(ImmutableList.of());
    }
}
