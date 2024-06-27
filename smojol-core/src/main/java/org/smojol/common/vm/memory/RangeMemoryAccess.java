package org.smojol.common.vm.memory;

public class RangeMemoryAccess implements MemoryAccess {
    private final MemoryRegion memoryRegion;
    private final int fromIndex;
    private final int toIndex;

    public RangeMemoryAccess(MemoryRegion memoryRegion, int fromIndex, int toIndex) {
        this.memoryRegion = memoryRegion;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
    }

    @Override
    public String toString() {
        return String.format("(%s-%s)", fromIndex, toIndex);
    }

    @Override
    public MemoryRegion get() {
        return memoryRegion.range(fromIndex, toIndex);
    }

    @Override
    public RangeMemoryAccess copy(int size) {
        return new RangeMemoryAccess(memoryRegion, fromIndex, fromIndex + size - 1);
    }

    @Override
    public int fromIndex() {
        return fromIndex;
    }

    @Override
    public MemoryRegion fullMemory() {
        return memoryRegion;
    }
}
