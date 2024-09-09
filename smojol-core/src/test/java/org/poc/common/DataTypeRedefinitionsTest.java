package org.poc.common;

import org.junit.jupiter.api.Test;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.memory.MemoryRegion;
import org.smojol.common.vm.memory.RangeMemoryAccess;
import org.smojol.common.vm.type.AlphanumericDataTypeSpec;
import org.smojol.common.vm.type.DataTypeSpec;
import org.smojol.common.vm.type.ZonedDecimalSignType;
import org.smojol.common.vm.type.ZonedDecimalDataTypeSpec;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.poc.common.MemoryTestUtils.assertMemory;

public class DataTypeRedefinitionsTest {
    @Test
    public void canHandlePartialRedefinitionsAsAlphanumerics() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(4, 0, ZonedDecimalSignType.UNSIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.set("1234");
        assertMemory(memoryRegion, "F1:F2:F3:F4");

        DataTypeSpec alphanumeric = new AlphanumericDataTypeSpec(4);
        MemoryLayout alphanumericLayout = new MemoryLayout(memoryRegion.fullAccess(), alphanumeric);
        assertEquals("1234", alphanumericLayout.read());

        DataTypeSpec smallerAlphanumeric = new AlphanumericDataTypeSpec(2);
        RangeMemoryAccess partialAccess = new RangeMemoryAccess(memoryRegion, 2, 3);
        MemoryLayout smallerAlphanumericLayout = new MemoryLayout(partialAccess, smallerAlphanumeric);
        assertEquals("34", smallerAlphanumericLayout.read());
        assertMemory(partialAccess.get(), "F3:F4");
    }

    @Test
    public void canHandleSmallerPartialRedefinitionsAsNumerics() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(4, 0, ZonedDecimalSignType.UNSIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.set("1234");
        assertMemory(memoryRegion, "F1:F2:F3:F4");

        DataTypeSpec shorterNumeric = new ZonedDecimalDataTypeSpec(2, 0);
        MemoryLayout shorterNumericLayout = new MemoryLayout(memoryRegion.fullAccess(), shorterNumeric);
        assertEquals(1234.0, shorterNumericLayout.read());

        DataTypeSpec smallerNumeric = new ZonedDecimalDataTypeSpec(2, 0);
        RangeMemoryAccess partialAccess = new RangeMemoryAccess(memoryRegion, 2, 3);
        MemoryLayout smallerNumericLayout = new MemoryLayout(partialAccess, smallerNumeric);
        assertEquals(34.0, smallerNumericLayout.read());
        assertMemory(partialAccess.get(), "F3:F4");
    }
}
