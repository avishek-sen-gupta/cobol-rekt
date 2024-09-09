package org.smojol.common.vm.type;

import org.smojol.common.vm.memory.MemoryAccess;
import org.smojol.common.vm.memory.MemoryRegion;

import java.util.List;

public enum Comp3SignType {
    UNSIGNED {
        @Override
        public char signNibble(boolean isPositive) {
            return 'F';
        }

        @Override
        public Object signed(double v, MemoryAccess access) {
            return v;
        }
    }, SIGNED {
        @Override
        public char signNibble(boolean isPositive) {
            return isPositive ? 'C' : 'D';
        }

        @Override
        public Object signed(double v, MemoryAccess access) {
            MemoryRegion memoryRegion = access.get();
            List<Byte> bytes = memoryRegion.asBytes();
            String lastByte = String.format("%02X", bytes.getLast());
            return lastByte.charAt(lastByte.length() - 1) == 'C' ? v : -v;
        }
    };

    public abstract char signNibble(boolean isPositive);
    public abstract Object signed(double v, MemoryAccess access);
}
