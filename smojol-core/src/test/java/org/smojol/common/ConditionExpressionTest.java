package org.smojol.common;

import org.junit.jupiter.api.Test;
import org.smojol.common.vm.memory.MemoryRegion;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ConditionExpressionTest {
    @Test
    public void canAllocateMemoryForNumbers() {
        MemoryRegion memoryRegion = new MemoryRegion("12345");
        assertEquals("12345", memoryRegion.asString());
        List<Byte> bytes = memoryRegion.asBytes();
        assertEquals((byte) '1', bytes.get(0));
        assertEquals((byte) '2', bytes.get(1));
        assertEquals((byte) '3', bytes.get(2));
        assertEquals((byte) '4', bytes.get(3));
        assertEquals((byte) '5', bytes.get(4));
        memoryRegion.set("67890");
        assertEquals("67890", memoryRegion.asString());
    }
}
