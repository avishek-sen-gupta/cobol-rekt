package org.smojol.common.vm.type;

import lombok.Getter;
import org.smojol.common.vm.exception.IllegalIndexException;
import org.smojol.common.vm.structure.ConversionStrategy;
import org.smojol.common.vm.memory.MemoryAccess;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.memory.MemoryRegion;

import java.util.List;

public class ZonedDecimalDataTypeSpec extends DataTypeSpec {
    public static final String BYTE_0X0 = Character.toString(0x00);
    public static final String CHARACTER_ZERO = "0";
    @Getter private final SignType signType;
    private final int leftSideDigits;
    private final int rightSideDigits;

    public ZonedDecimalDataTypeSpec(int leftSideDigits, int rightSideDigits) {
        this(leftSideDigits, rightSideDigits, SignType.UNSIGNED);
    }

    public ZonedDecimalDataTypeSpec(int leftSideDigits, int rightSideDigits, SignType signType) {
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
        String s = numberAsString(access);
        return Double.parseDouble(s);
    }

    private String numberAsString(MemoryAccess access) {
        MemoryRegion region = access.get();
        return digits(region).stream().map(b -> Byte.toString((byte) (b & 0x0F))).reduce("", (a, b) -> a + b);
    }

    @Override
    public Object readFormatted(MemoryAccess access) {
        return signType.signed(Double.parseDouble(new StringBuilder(numberAsString(access)).insert(leftSideDigits, ".").toString()), access);
    }

    @Override
    public void refresh(MemoryAccess access) {
        MemoryRegion region = access.get();
        List<Byte> digits = digits(region);
        boolean isPositive = signType == SignType.UNSIGNED
                || (signType == SignType.SIGNED && ((region.asBytes().getLast() & 0xF0) != 0xD0));

        String overflowingNumber = digits.stream().map(this::hexChar).reduce((a, b) -> a + b).get();
        int zoned = MemoryRegion.zoned(overflowingNumber);
        String zonedFiltered = readThroughFilter(String.valueOf(zoned));
        region.write(signType.toBytes(zonedFiltered, isPositive));
    }

    @Override
    public void set(String s, MemoryAccess access) {
        MemoryRegion region = access.get();
        boolean isPositive = ConversionStrategy.asNumber(s) >= 0;
        String magnitudeString = readThroughFilter(s.replace("-", ""));
        List<Byte> bytes = signType.toBytes(magnitudeString, isPositive);
        region.write(bytes);
    }

    @Override
    public MemoryLayout index(int i, MemoryAccess access) {
        throw new IllegalIndexException(i, "Non-TableSpec type");
    }

    private String hexChar(Byte b) {
        if (b.equals((byte) 0x00)) return "0";
        else if (b.equals((byte) 0x01)) return "1";
        else if (b.equals((byte) 0x02)) return "2";
        else if (b.equals((byte) 0x03)) return "3";
        else if (b.equals((byte) 0x04)) return "4";
        else if (b.equals((byte) 0x05)) return "5";
        else if (b.equals((byte) 0x06)) return "6";
        else if (b.equals((byte) 0x07)) return "7";
        else if (b.equals((byte) 0x08)) return "8";
        else if (b.equals((byte) 0x09)) return "9";
        else if (b.equals((byte) 0x0A)) return "A";
        else if (b.equals((byte) 0x0B)) return "B";
        else if (b.equals((byte) 0x0C)) return "C";
        else if (b.equals((byte) 0x0D)) return "D";
        else if (b.equals((byte) 0x0E)) return "E";
        else if (b.equals((byte) 0x0F)) return "F";

        throw new IllegalArgumentException("Shouldn't be greater than 0x0F");
    }

    private List<Byte> digits(MemoryRegion region) {
        return region.asBytes().stream().map(b -> (byte) (b & 0x0F)).toList();
    }
}
