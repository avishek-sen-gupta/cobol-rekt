package org.smojol.common;

import org.smojol.common.vm.memory.MemoryRegion;

import java.util.HexFormat;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class MemoryTestUtils {
    public static void assertBytes(List<Byte> actualBytes, String expectedByteString) {
        byte[] expectedBytes = HexFormat.ofDelimiter(":").parseHex(expectedByteString);
        assertEquals(expectedBytes.length, actualBytes.size(), "Expected and actual byte arrays differ in length");
        for (int i = 0; i <= expectedBytes.length - 1; ++i) {
            assertEquals(expectedBytes[i], actualBytes.get(i), String.format("Expected %02X (%s) but got %02X (%s) at position %s", expectedBytes[i], expectedByteString, actualBytes.get(i), MemoryRegion.byteString(actualBytes), i));
        }
    }

    public static void assertMemory(MemoryRegion memoryRegion, String expectedByteString) {
        assertBytes(memoryRegion.asBytes(), expectedByteString);
    }
}
