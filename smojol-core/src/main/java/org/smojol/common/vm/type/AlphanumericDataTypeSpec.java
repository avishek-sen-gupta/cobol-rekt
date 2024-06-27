package org.smojol.common.vm.type;

import org.smojol.common.vm.exception.IllegalIndexException;
import org.smojol.common.vm.memory.MemoryAccess;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.memory.MemoryRegion;
import org.smojol.common.vm.expression.RightAdjuster;

import static org.smojol.common.vm.memory.MemoryLayout.toBytes;

public class AlphanumericDataTypeSpec extends DataTypeSpec {
    public AlphanumericDataTypeSpec(int maxLength) {
        super(new RightAdjuster(maxLength));
    }

    @Override
    public String readRaw(String v) {
        return readThroughFilter(v);
    }

    public String readPublic(MemoryAccess access) {
        MemoryRegion region = access.get();
        return region.asEbcdicBytes().stream().map(b -> String.valueOf((char) b.byteValue())).reduce("", (a, b) -> a + b);
    }

    @Override
    public Object readFormatted(MemoryAccess access) {
        return readPublic(access);
    }

    @Override
    public void refresh(MemoryAccess access) {

    }

    @Override
    public void set(String s, MemoryAccess access) {
        MemoryRegion region = access.get();
        region.writeEbcdic(toBytes(readThroughFilter(s)));
    }

    @Override
    public MemoryLayout index(int i, MemoryAccess access) {
        throw new IllegalIndexException(i, "Non-TableSpec type");
    }
}
