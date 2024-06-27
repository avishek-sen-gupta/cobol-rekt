package org.smojol.common.vm.type;

public interface DataFilter {
    String filter(String s);
    int sizeInBytes();
}
