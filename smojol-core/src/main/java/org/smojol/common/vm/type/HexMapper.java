package org.smojol.common.vm.type;

import java.util.List;

public class HexMapper {
    static String hexChar(Byte b) {
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

    public static List<Byte> asBytes(List<Integer> integers) {
        return integers.stream().map(i -> Byte.valueOf(i.toString())).toList();
    }

    public static Byte hexStringToByte(char msw, char lsw) {
        return (byte) Integer.parseInt(String.valueOf(msw) + lsw, 16);
    }
}
