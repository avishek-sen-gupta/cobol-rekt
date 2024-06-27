package org.smojol.common.vm.memory;

public interface MemoryAccess {
    int fromIndex();
    MemoryRegion get();
    MemoryRegion fullMemory();
    RangeMemoryAccess copy(int size);
}
