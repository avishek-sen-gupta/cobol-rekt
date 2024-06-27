package org.smojol.common.vm.type;

import org.smojol.common.vm.memory.MemoryAccess;
import org.smojol.common.vm.memory.MemoryLayout;

public abstract class DataTypeSpec {
    protected final DataFilter filter;

    public abstract String readRaw(String v);
    // This is for reading values in memory as-is without formatting (like decimal points for numbers)
    public abstract Object readPublic(MemoryAccess access);
    // This is for reading values in memory with formatting (like decimal points for numbers)
    public abstract Object readFormatted(MemoryAccess access);
    public abstract void refresh(MemoryAccess access);
    public abstract void set(String s, MemoryAccess access);
    public abstract MemoryLayout index(int i, MemoryAccess access);

    public DataTypeSpec(DataFilter filter) {
        this.filter = filter;
    }

    public String readThroughFilter(String v) {
        return filter.filter(v);
    }

    public int sizeInBytes() {
        return filter.sizeInBytes();
    }
}
