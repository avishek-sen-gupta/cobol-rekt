package org.smojol.common.vm.type;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.vm.exception.IllegalIndexException;
import org.smojol.common.vm.memory.MemoryAccess;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.memory.MemoryRegion;
import org.smojol.common.vm.structure.ConversionStrategy;

import java.util.ArrayList;
import java.util.List;

public class Comp3lDataTypeSpec extends DataTypeSpec {
    public static final String BYTE_0X0 = Character.toString(0x00);
    public static final String CHARACTER_ZERO = "0";
    @Getter
    private final Comp3SignType signType;
    private final int leftSideDigits;
    private final int rightSideDigits;

    public Comp3lDataTypeSpec(int leftSideDigits, int rightSideDigits) {
        this(leftSideDigits, rightSideDigits, Comp3SignType.UNSIGNED);
    }

    public Comp3lDataTypeSpec(int leftSideDigits, int rightSideDigits, Comp3SignType signType) {
        super(new DecimalPointAligner(leftSideDigits, rightSideDigits));
        this.signType = signType;
        this.leftSideDigits = leftSideDigits;
        this.rightSideDigits = rightSideDigits;
    }

    @Override
    public String readRaw(String v) {
        return new LeftAdjuster(sizeInBytes(), BYTE_0X0).filter(v);
    }

    public Object readPublic(MemoryAccess access) {
        return signType.signed(Double.parseDouble(numberAsString(access)), access);
    }

    private String numberAsString(MemoryAccess access) {
        MemoryRegion memoryRegion = access.get();
        List<Byte> bytes = memoryRegion.asBytes();
        String fullByteStringWithSign = bytes.stream().map(b -> String.format("%02X", b)).reduce("", (byteString, b) -> byteString + b);
        return fullByteStringWithSign.substring(0, fullByteStringWithSign.length() - 1);
    }

    @Override
    public Object readFormatted(MemoryAccess access) {
        return signType.signed(Double.parseDouble(new StringBuilder(numberAsString(access)).insert(leftSideDigits + 1, ".").toString()), access);
    }

    @Override
    public void refresh(MemoryAccess access) {
    }

    @Override
    public void set(String s, MemoryAccess access) {
        MemoryRegion region = access.get();
        boolean isPositive = ConversionStrategy.asNumber(s) >= 0;
        String magnitudeString = readThroughFilter(s.replace("-", ""));
        char lastDigit = magnitudeString.charAt(magnitudeString.length() - 1);
        String remainingDigits = magnitudeString.substring(0, magnitudeString.length() - 1);
        List<Byte> allBytes = recurse(new StringBuilder(remainingDigits).reverse().toString()).reversed();
        allBytes.add(HexMapper.hexStringToByte(lastDigit, signType.signNibble(isPositive)));
        region.write(allBytes);
    }

    public List<Byte> recurse(String remainingDigits) {
        if (remainingDigits.isEmpty()) return ImmutableList.of();
        if (remainingDigits.length() == 1) {
            return ImmutableList.of(HexMapper.hexStringToByte('0', remainingDigits.charAt(0)));
        }
        Byte currentByte = HexMapper.hexStringToByte(remainingDigits.charAt(1), remainingDigits.charAt(0));
        if (remainingDigits.length() == 2) {
            return ImmutableList.of(currentByte);
        }

        List<Byte> remainingBytes = recurse(remainingDigits.substring(2));
        List<Byte> all = new ArrayList<>();
        all.add(currentByte);
        all.addAll(remainingBytes);

        return all;
    }

    @Override
    public MemoryLayout index(int i, MemoryAccess access) {
        throw new IllegalIndexException(i, "Non-TableSpec type");
    }

    private List<Byte> digits(MemoryRegion region) {
        return region.asBytes().stream().map(b -> (byte) (b & 0x0F)).toList();
    }

    @Override
    public int sizeInBytes() {
        int totalDigits = leftSideDigits + rightSideDigits;
        return totalDigits / 2 + 1;
    }
}
