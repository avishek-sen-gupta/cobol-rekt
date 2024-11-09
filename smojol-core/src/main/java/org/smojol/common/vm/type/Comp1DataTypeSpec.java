package org.smojol.common.vm.type;

import org.smojol.common.vm.memory.MemoryAccess;
import org.smojol.common.vm.memory.MemoryLayout;

public class Comp1DataTypeSpec extends DataTypeSpec {
    public Comp1DataTypeSpec() {
        super(DataFilter.NO_FILTER);
    }

    @Override
    public String readRaw(String v) {
        throw new UnsupportedOperationException("COMP-1 operations not supported yet!");
    }

    @Override
    public Object readPublic(MemoryAccess access) {
        throw new UnsupportedOperationException("COMP-1 operations not supported yet!");
    }

    @Override
    public Object readFormatted(MemoryAccess access) {
        throw new UnsupportedOperationException("COMP-1 operations not supported yet!");
    }

    @Override
    public void refresh(MemoryAccess access) {
        throw new UnsupportedOperationException("COMP-1 operations not supported yet!");
    }

    @Override
    public void set(String s, MemoryAccess access) {
        throw new UnsupportedOperationException("COMP-1 operations not supported yet!");
    }

    @Override
    public MemoryLayout index(int i, MemoryAccess access) {
        throw new UnsupportedOperationException("COMP-1 operations not supported yet!");
    }
}
