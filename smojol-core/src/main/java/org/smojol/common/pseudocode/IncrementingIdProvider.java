package org.smojol.common.pseudocode;

import org.smojol.common.id.IdProvider;

public class IncrementingIdProvider implements IdProvider {
    int count = 1;
    @Override
    public String next() {
        return "T" + (count++);
    }
}
