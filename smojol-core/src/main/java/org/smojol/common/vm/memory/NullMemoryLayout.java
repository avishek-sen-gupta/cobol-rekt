package org.smojol.common.vm.memory;

public class NullMemoryLayout extends MemoryLayout {
    public NullMemoryLayout() {
        super(null, null);
    }

    @Override
    public String toString() {
        return "";
    }

    public MemoryRegion memory() {
        throw new UnsupportedOperationException("Null MemoryLayout has no memory region");
    }

    public void set(String v) {
    }

    public void refresh() {
    }

    public Object read() {
        return new Object();
    }

    public void setRaw(String s) {
    }

    public Object readFormatted() {
        return new Object();
    }

    public NullMemoryLayout index(int i) {
        return this;
    }
}
