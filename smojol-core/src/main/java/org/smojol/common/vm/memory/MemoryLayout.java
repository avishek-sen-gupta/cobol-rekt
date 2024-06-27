package org.smojol.common.vm.memory;

import lombok.Getter;
import org.smojol.common.vm.type.DataTypeSpec;

import java.util.ArrayList;
import java.util.List;

public class MemoryLayout {
    @Getter private final MemoryAccess access;
    @Getter private final DataTypeSpec typeSpec;

    public MemoryLayout(MemoryAccess access, DataTypeSpec typeSpec) {
        this.access = access;
        this.typeSpec = typeSpec;
    }

    @Override
    public String toString() {
        return access.toString();
    }

    public MemoryRegion memory() {
        return access.get();
    }

    public static List<Byte> toUnsignedBytes(int number) {
        String current = String.valueOf(number);
        return toUnsignedBytes(current);
    }

    public static List<Byte> toSignedBytes(int number) {
        List<Byte> bytes = toUnsignedBytes(number);
        Byte last = bytes.getLast();
        bytes.set(bytes.size() - 1, (byte) (number < 0 ? last - 0x20 : last - 0x30));
        return bytes;
    }

    public static List<Byte> toUnsignedBytes(String magnitudeString) {
        List<Byte> bytes = new ArrayList<>();
        for (int i = 0; i < magnitudeString.length(); i++) {
            if (magnitudeString.charAt(i) < '0' || magnitudeString.charAt(i) > '9') continue;
            bytes.add((byte) (0xF0 + Integer.valueOf(magnitudeString.substring(i, i + 1)).byteValue()));
        }
        return bytes;
    }

    public static List<Byte> toSignedBytes(String magnitudeString, boolean isPositive) {
        List<Byte> bytes = toUnsignedBytes(magnitudeString);
        Byte last = bytes.getLast();
        bytes.set(bytes.size() - 1, (byte) (isPositive ? last - 0x30 : last - 0x20));
        return bytes;
    }

    public void set(String v) {
        typeSpec.set(v, access);
    }

    public static List<Byte> toBytes(String text) {
        List<Byte> bytes = new ArrayList<>();
        for (int i = 0; i < text.length(); i++) {
            bytes.add((byte) text.charAt(i));
        }
        return bytes;
    }

    private int digitsFromZonedDecimal() {
        MemoryRegion region = access.get();
        String numberAsString = region.asBytes().stream().map(b -> Byte.toString((byte) (b & 0x0F))).reduce("", (a, b) -> a + b);
        return Integer.parseInt(numberAsString);
    }

    public void refresh() {
        typeSpec.refresh(access);
    }

    public Object read() {
        return typeSpec.readPublic(access);
    }

    public void setRaw(String s) {
        MemoryRegion region = access.get();
        region.write(toBytes(typeSpec.readRaw(s)));
    }

    public Object readFormatted() {
        return typeSpec.readFormatted(access);
    }

    public MemoryLayout index(int i) {
        return typeSpec.index(i, access);
    }
}
