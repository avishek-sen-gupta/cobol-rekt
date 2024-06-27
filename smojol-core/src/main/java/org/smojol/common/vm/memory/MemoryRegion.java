package org.smojol.common.vm.memory;

import org.apache.commons.codec.binary.Hex;
import org.apache.commons.lang3.ArrayUtils;
import org.smojol.common.vm.type.CobolDataType;

import java.nio.charset.StandardCharsets;
import java.util.*;

public class MemoryRegion {
    protected List<Byte> blob;
    protected CobolDataType internalRepresentation;

    public MemoryRegion(String text) {
        this(toBytes(text), CobolDataType.STRING);
    }

    protected MemoryRegion(List<Byte> blob, CobolDataType internalRepresentation) {
        this.blob = blob;
        this.internalRepresentation = internalRepresentation;
    }

    public MemoryRegion(int sizeInBytes) {
        this.blob = new ArrayList<>(sizeInBytes);
        for (int i = 0; i < sizeInBytes; i++) {
            blob.add((byte) 0);
        }
    }

    private static List<Byte> toBytes(String text) {
        List<Byte> bytes = new ArrayList<>();
        for (int i = 0; i < text.length(); i++) {
            bytes.add((byte) text.charAt(i));
        }
        return bytes;
    }

    private static List<Byte> toBytes(int number) {
        String current = String.valueOf(number);
        List<Byte> bytes = new ArrayList<>();
        for (int i = 0; i < current.length(); i++) {
            bytes.add(Integer.valueOf(current.substring(i, i + 1)).byteValue());
        }
        return bytes;
    }

    public static int zoned(String s) {
        List<Map.Entry<Character, Integer>> positions = new ArrayList<>();
        for (int i = s.length() - 1; i >= 0; i--) {
            if (s.charAt(i) >= '0' && s.charAt(i) <= '9') continue;
            positions.add(new AbstractMap.SimpleImmutableEntry<>(s.charAt(i), s.length() - 1 - i));
        }

        Integer delta = positions.stream().map(e -> (int) ((e.getKey() - '@') * Math.pow(10, e.getValue()))).reduce(0, Integer::sum);
        String plainNumber = s.replace('A', '9')
                .replace('B', '9')
                .replace('C', '9')
                .replace('D', '9')
                .replace('E', '9')
                .replace('F', '9');

        return Integer.parseInt(plainNumber) + delta;
    }

    public String asString() {
        List<Byte> retrievedBlock = blob;
        byte[] array = ArrayUtils.toPrimitive(retrievedBlock.toArray(new Byte[0]));
        return new String(array, StandardCharsets.UTF_8);
    }

    public void set(String text) {
        this.blob = toBytes(text);
        internalRepresentation = CobolDataType.STRING;
    }

    public MemoryRegion range(int fromIndex, int toIndex) {
        return new MemoryRegion(blob.subList(fromIndex, toIndex + 1), internalRepresentation);
    }

    public List<Byte> asBytes() {
        return new ArrayList<>(blob);
    }

    public List<Byte> asEbcdicBytes() {
        ByteConverter c = new ByteConverter();
        List<Byte> ascii = blob.stream().map(c::ebcdicToAscii).toList();
        return ascii;
    }

    public void writeEbcdic(List<Byte> bytes) {
        ByteConverter c = new ByteConverter();
        List<Byte> ebcdicBytes = bytes.stream().map(c::asciiToEbcdic).toList();
        write(ebcdicBytes);
    }

    public MemoryAccess fullAccess() {
        return new RangeMemoryAccess(this, 0, blob.size() - 1);
    }

    public void write(List<Byte> bytes) {
        Collections.copy(blob, bytes);
    }

    public String getByteString() {
        return byteString(blob);
    }

    public static String byteString(List<Byte> bytes) {
        return String.join(":", bytes.stream().map(b -> Hex.encodeHexString(new byte[] {b.byteValue()}).toUpperCase()).toList());
    }
}
