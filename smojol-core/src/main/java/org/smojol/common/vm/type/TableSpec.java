package org.smojol.common.vm.type;

import org.smojol.common.vm.exception.IllegalIndexException;
import org.smojol.common.vm.memory.MemoryAccess;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.memory.MemoryRegion;
import org.smojol.common.vm.memory.RangeMemoryAccess;

public class TableSpec extends DataTypeSpec {
    private final DataTypeSpec coreType;
    private final int size;

    public TableSpec(DataTypeSpec coreType, int size) {
        super(null);
        this.coreType = coreType;
        this.size = size;
    }

    @Override
    public String readRaw(String v) {
        return "";
    }

    @Override
    public Object readPublic(MemoryAccess access) {
        return null;
    }

    @Override
    public Object readFormatted(MemoryAccess access) {
        return null;
    }

    @Override
    public void refresh(MemoryAccess access) {

    }

    @Override
    public void set(String s, MemoryAccess access) {

    }

    @Override
    public int sizeInBytes() {
        return coreType.sizeInBytes() * size;
    }

    @Override
    public MemoryLayout index(int index, MemoryAccess access) {
        if (index < 0 || index >= coreType.sizeInBytes()) throw new IllegalIndexException(index, "Index out of bounds");
        MemoryRegion region = access.get();
        RangeMemoryAccess subscriptAccess = new RangeMemoryAccess(region, index * coreType.sizeInBytes(), index * coreType.sizeInBytes() + coreType.sizeInBytes() - 1);
        return new MemoryLayout(subscriptAccess, coreType);
    }
}
