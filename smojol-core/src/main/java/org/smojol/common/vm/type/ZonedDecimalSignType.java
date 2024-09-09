package org.smojol.common.vm.type;

import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.vm.memory.MemoryAccess;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.memory.MemoryRegion;

import java.util.List;
import java.util.logging.Logger;

public enum ZonedDecimalSignType {
    UNSIGNED {
        @Override
        public List<Byte> toBytes(String magnitudeString, boolean isPositive) {
            return MemoryLayout.toUnsignedBytes(magnitudeString);
        }

        @Override
        public Double signedNumber(double v, MemoryAccess access) {
            return v;
        }
    }, SIGNED {
        @Override
        public List<Byte> toBytes(String magnitudeString, boolean isPositive) {
            return MemoryLayout.toSignedBytes(magnitudeString, isPositive);
        }

        @Override
        public Double signedNumber(double v, MemoryAccess access) {
            MemoryRegion region = access.get();
            int signByte = region.asBytes().getLast() & 0xF0;
            if (signByte == 0xD0) return -v;
            else if (signByte == 0xC0) return v;
            java.util.logging.Logger logger = Logger.getLogger(ZonedDecimalSignType.class.getName());
            logger.warning(ConsoleColors.coloured("Unknown sign bit in signed type, possible bug in set() of ZonedDecimalType", 0, 202));
            return v;
        }
    };

    public abstract List<Byte> toBytes(String magnitudeString, boolean isPositive);
    public abstract Double signedNumber(double v, MemoryAccess access);

    public Double signed(double v, MemoryAccess access) {
        if (v < 0) {
            java.util.logging.Logger logger = Logger.getLogger(ZonedDecimalSignType.class.getName());
            logger.warning(ConsoleColors.red("Value is already negative. Potential misrepresentation of memory region!"));
            return v;
        }
        return signedNumber(v, access);
    }
}
