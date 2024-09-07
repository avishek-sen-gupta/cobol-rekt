package org.smojol.common.vm.memory;

public class ByteConverter {
    public static byte[] ASCII2EBCDIC = new byte[256];
    public static byte[] EBCDIC2ASCII = new byte[256];

    static {
        ASCII2EBCDIC[0x00] = (byte) 0x00;
        ASCII2EBCDIC[0x01] = (byte) 0x01;
        ASCII2EBCDIC[0x02] = (byte) 0x02;
        ASCII2EBCDIC[0x03] = (byte) 0x03;
        ASCII2EBCDIC[0x04] = (byte) 0x37;
        ASCII2EBCDIC[0x05] = (byte) 0x2D;
        ASCII2EBCDIC[0x06] = (byte) 0x2E;
        ASCII2EBCDIC[0x07] = (byte) 0x2F;
        ASCII2EBCDIC[0x08] = (byte) 0x16;
        ASCII2EBCDIC[0x09] = (byte) 0x05;
        ASCII2EBCDIC[0x0A] = (byte) 0x25;
        ASCII2EBCDIC[0x0B] = (byte) 0x0B;
        ASCII2EBCDIC[0x0C] = (byte) 0x0C;
        ASCII2EBCDIC[0x0D] = (byte) 0x0D;
        ASCII2EBCDIC[0x0E] = (byte) 0x0E;
        ASCII2EBCDIC[0x0F] = (byte) 0x0F;
        ASCII2EBCDIC[0x10] = (byte) 0x10;
        ASCII2EBCDIC[0x11] = (byte) 0x11;
        ASCII2EBCDIC[0x12] = (byte) 0x12;
        ASCII2EBCDIC[0x13] = (byte) 0x13;
        ASCII2EBCDIC[0x14] = (byte) 0x3C;
        ASCII2EBCDIC[0x15] = (byte) 0x3D;
        ASCII2EBCDIC[0x16] = (byte) 0x32;
        ASCII2EBCDIC[0x17] = (byte) 0x26;
        ASCII2EBCDIC[0x18] = (byte) 0x18;
        ASCII2EBCDIC[0x19] = (byte) 0x19;
        ASCII2EBCDIC[0x1A] = (byte) 0x3F;
        ASCII2EBCDIC[0x1B] = (byte) 0x27;
        ASCII2EBCDIC[0x1C] = (byte) 0x1C;
        ASCII2EBCDIC[0x1D] = (byte) 0x1D;
        ASCII2EBCDIC[0x1E] = (byte) 0x1E;
        ASCII2EBCDIC[0x1F] = (byte) 0x1F;
        ASCII2EBCDIC[0x20] = (byte) 0x40;
        ASCII2EBCDIC[0x21] = (byte) 0x4F;
        ASCII2EBCDIC[0x22] = (byte) 0x7F;
        ASCII2EBCDIC[0x23] = (byte) 0x7B;
        ASCII2EBCDIC[0x24] = (byte) 0x5B;
        ASCII2EBCDIC[0x25] = (byte) 0x6C;
        ASCII2EBCDIC[0x26] = (byte) 0x50;
        ASCII2EBCDIC[0x27] = (byte) 0x7D;
        ASCII2EBCDIC[0x28] = (byte) 0x4D;
        ASCII2EBCDIC[0x29] = (byte) 0x5D;
        ASCII2EBCDIC[0x2A] = (byte) 0x5C;
        ASCII2EBCDIC[0x2B] = (byte) 0x4E;
        ASCII2EBCDIC[0x2C] = (byte) 0x6B;
        ASCII2EBCDIC[0x2D] = (byte) 0x60;
        ASCII2EBCDIC[0x2E] = (byte) 0x4B;
        ASCII2EBCDIC[0x2F] = (byte) 0x61;
        ASCII2EBCDIC[0x30] = (byte) 0xF0;
        ASCII2EBCDIC[0x31] = (byte) 0xF1;
        ASCII2EBCDIC[0x32] = (byte) 0xF2;
        ASCII2EBCDIC[0x33] = (byte) 0xF3;
        ASCII2EBCDIC[0x34] = (byte) 0xF4;
        ASCII2EBCDIC[0x35] = (byte) 0xF5;
        ASCII2EBCDIC[0x36] = (byte) 0xF6;
        ASCII2EBCDIC[0x37] = (byte) 0xF7;
        ASCII2EBCDIC[0x38] = (byte) 0xF8;
        ASCII2EBCDIC[0x39] = (byte) 0xF9;
        ASCII2EBCDIC[0x3A] = (byte) 0x7A;
        ASCII2EBCDIC[0x3B] = (byte) 0x5E;
        ASCII2EBCDIC[0x3C] = (byte) 0x4C;
        ASCII2EBCDIC[0x3D] = (byte) 0x7E;
        ASCII2EBCDIC[0x3E] = (byte) 0x6E;
        ASCII2EBCDIC[0x3F] = (byte) 0x6F;
        ASCII2EBCDIC[0x40] = (byte) 0x7C;
        ASCII2EBCDIC[0x41] = (byte) 0xC1;
        ASCII2EBCDIC[0x42] = (byte) 0xC2;
        ASCII2EBCDIC[0x43] = (byte) 0xC3;
        ASCII2EBCDIC[0x44] = (byte) 0xC4;
        ASCII2EBCDIC[0x45] = (byte) 0xC5;
        ASCII2EBCDIC[0x46] = (byte) 0xC6;
        ASCII2EBCDIC[0x47] = (byte) 0xC7;
        ASCII2EBCDIC[0x48] = (byte) 0xC8;
        ASCII2EBCDIC[0x49] = (byte) 0xC9;
        ASCII2EBCDIC[0x4A] = (byte) 0xD1;
        ASCII2EBCDIC[0x4B] = (byte) 0xD2;
        ASCII2EBCDIC[0x4C] = (byte) 0xD3;
        ASCII2EBCDIC[0x4D] = (byte) 0xD4;
        ASCII2EBCDIC[0x4E] = (byte) 0xD5;
        ASCII2EBCDIC[0x4F] = (byte) 0xD6;
        ASCII2EBCDIC[0x50] = (byte) 0xD7;
        ASCII2EBCDIC[0x51] = (byte) 0xD8;
        ASCII2EBCDIC[0x52] = (byte) 0xD9;
        ASCII2EBCDIC[0x53] = (byte) 0xE2;
        ASCII2EBCDIC[0x54] = (byte) 0xE3;
        ASCII2EBCDIC[0x55] = (byte) 0xE4;
        ASCII2EBCDIC[0x56] = (byte) 0xE5;
        ASCII2EBCDIC[0x57] = (byte) 0xE6;
        ASCII2EBCDIC[0x58] = (byte) 0xE7;
        ASCII2EBCDIC[0x59] = (byte) 0xE8;
        ASCII2EBCDIC[0x5A] = (byte) 0xE9;
        ASCII2EBCDIC[0x5B] = (byte) 0x4A;
        ASCII2EBCDIC[0x5C] = (byte) 0xE0;
        ASCII2EBCDIC[0x5D] = (byte) 0x5A;
        ASCII2EBCDIC[0x5E] = (byte) 0x5F;
        ASCII2EBCDIC[0x5F] = (byte) 0x6D;
        ASCII2EBCDIC[0x60] = (byte) 0x79;
        ASCII2EBCDIC[0x61] = (byte) 0x81;
        ASCII2EBCDIC[0x62] = (byte) 0x82;
        ASCII2EBCDIC[0x63] = (byte) 0x83;
        ASCII2EBCDIC[0x64] = (byte) 0x84;
        ASCII2EBCDIC[0x65] = (byte) 0x85;
        ASCII2EBCDIC[0x66] = (byte) 0x86;
        ASCII2EBCDIC[0x67] = (byte) 0x87;
        ASCII2EBCDIC[0x68] = (byte) 0x88;
        ASCII2EBCDIC[0x69] = (byte) 0x89;
        ASCII2EBCDIC[0x6A] = (byte) 0x91;
        ASCII2EBCDIC[0x6B] = (byte) 0x92;
        ASCII2EBCDIC[0x6C] = (byte) 0x93;
        ASCII2EBCDIC[0x6D] = (byte) 0x94;
        ASCII2EBCDIC[0x6E] = (byte) 0x95;
        ASCII2EBCDIC[0x6F] = (byte) 0x96;
        ASCII2EBCDIC[0x70] = (byte) 0x97;
        ASCII2EBCDIC[0x71] = (byte) 0x98;
        ASCII2EBCDIC[0x72] = (byte) 0x99;
        ASCII2EBCDIC[0x73] = (byte) 0xA2;
        ASCII2EBCDIC[0x74] = (byte) 0xA3;
        ASCII2EBCDIC[0x75] = (byte) 0xA4;
        ASCII2EBCDIC[0x76] = (byte) 0xA5;
        ASCII2EBCDIC[0x77] = (byte) 0xA6;
        ASCII2EBCDIC[0x78] = (byte) 0xA7;
        ASCII2EBCDIC[0x79] = (byte) 0xA8;
        ASCII2EBCDIC[0x7A] = (byte) 0xA9;
        ASCII2EBCDIC[0x7B] = (byte) 0xC0;
        ASCII2EBCDIC[0x7C] = (byte) 0x6A;
        ASCII2EBCDIC[0x7D] = (byte) 0xD0;
        ASCII2EBCDIC[0x7E] = (byte) 0xA1;
        ASCII2EBCDIC[0x7F] = (byte) 0x07;
        ASCII2EBCDIC[0x80] = (byte) 0x20;
        ASCII2EBCDIC[0x81] = (byte) 0x21;
        ASCII2EBCDIC[0x82] = (byte) 0x22;
        ASCII2EBCDIC[0x83] = (byte) 0x23;
        ASCII2EBCDIC[0x84] = (byte) 0x24;
        ASCII2EBCDIC[0x85] = (byte) 0x15;
        ASCII2EBCDIC[0x86] = (byte) 0x06;
        ASCII2EBCDIC[0x87] = (byte) 0x17;
        ASCII2EBCDIC[0x88] = (byte) 0x28;
        ASCII2EBCDIC[0x89] = (byte) 0x29;
        ASCII2EBCDIC[0x8A] = (byte) 0x2A;
        ASCII2EBCDIC[0x8B] = (byte) 0x2B;
        ASCII2EBCDIC[0x8C] = (byte) 0x2C;
        ASCII2EBCDIC[0x8D] = (byte) 0x09;
        ASCII2EBCDIC[0x8E] = (byte) 0x0A;
        ASCII2EBCDIC[0x8F] = (byte) 0x1B;
        ASCII2EBCDIC[0x90] = (byte) 0x30;
        ASCII2EBCDIC[0x91] = (byte) 0x31;
        ASCII2EBCDIC[0x92] = (byte) 0x1A;
        ASCII2EBCDIC[0x93] = (byte) 0x33;
        ASCII2EBCDIC[0x94] = (byte) 0x34;
        ASCII2EBCDIC[0x95] = (byte) 0x35;
        ASCII2EBCDIC[0x96] = (byte) 0x36;
        ASCII2EBCDIC[0x97] = (byte) 0x08;
        ASCII2EBCDIC[0x98] = (byte) 0x38;
        ASCII2EBCDIC[0x99] = (byte) 0x39;
        ASCII2EBCDIC[0x9A] = (byte) 0x3A;
        ASCII2EBCDIC[0x9B] = (byte) 0x3B;
        ASCII2EBCDIC[0x9C] = (byte) 0x04;
        ASCII2EBCDIC[0x9D] = (byte) 0x14;
        ASCII2EBCDIC[0x9E] = (byte) 0x3E;
        ASCII2EBCDIC[0x9F] = (byte) 0xE1;
        ASCII2EBCDIC[0xA0] = (byte) 0x41;
        ASCII2EBCDIC[0xA1] = (byte) 0x42;
        ASCII2EBCDIC[0xA2] = (byte) 0x43;
        ASCII2EBCDIC[0xA3] = (byte) 0x44;
        ASCII2EBCDIC[0xA4] = (byte) 0x45;
        ASCII2EBCDIC[0xA5] = (byte) 0x46;
        ASCII2EBCDIC[0xA6] = (byte) 0x47;
        ASCII2EBCDIC[0xA7] = (byte) 0x48;
        ASCII2EBCDIC[0xA8] = (byte) 0x49;
        ASCII2EBCDIC[0xA9] = (byte) 0x51;
        ASCII2EBCDIC[0xAA] = (byte) 0x52;
        ASCII2EBCDIC[0xAB] = (byte) 0x53;
        ASCII2EBCDIC[0xAC] = (byte) 0x54;
        ASCII2EBCDIC[0xAD] = (byte) 0x55;
        ASCII2EBCDIC[0xAE] = (byte) 0x56;
        ASCII2EBCDIC[0xAF] = (byte) 0x57;
        ASCII2EBCDIC[0xB0] = (byte) 0x58;
        ASCII2EBCDIC[0xB1] = (byte) 0x59;
        ASCII2EBCDIC[0xB2] = (byte) 0x62;
        ASCII2EBCDIC[0xB3] = (byte) 0x63;
        ASCII2EBCDIC[0xB4] = (byte) 0x64;
        ASCII2EBCDIC[0xB5] = (byte) 0x65;
        ASCII2EBCDIC[0xB6] = (byte) 0x66;
        ASCII2EBCDIC[0xB7] = (byte) 0x67;
        ASCII2EBCDIC[0xB8] = (byte) 0x68;
        ASCII2EBCDIC[0xB9] = (byte) 0x69;
        ASCII2EBCDIC[0xBA] = (byte) 0x70;
        ASCII2EBCDIC[0xBB] = (byte) 0x71;
        ASCII2EBCDIC[0xBC] = (byte) 0x72;
        ASCII2EBCDIC[0xBD] = (byte) 0x73;
        ASCII2EBCDIC[0xBE] = (byte) 0x74;
        ASCII2EBCDIC[0xBF] = (byte) 0x75;
        ASCII2EBCDIC[0xC0] = (byte) 0x76;
        ASCII2EBCDIC[0xC1] = (byte) 0x77;
        ASCII2EBCDIC[0xC2] = (byte) 0x78;
        ASCII2EBCDIC[0xC3] = (byte) 0x80;
        ASCII2EBCDIC[0xC4] = (byte) 0x8A;
        ASCII2EBCDIC[0xC5] = (byte) 0x8B;
        ASCII2EBCDIC[0xC6] = (byte) 0x8C;
        ASCII2EBCDIC[0xC7] = (byte) 0x8D;
        ASCII2EBCDIC[0xC8] = (byte) 0x8E;
        ASCII2EBCDIC[0xC9] = (byte) 0x8F;
        ASCII2EBCDIC[0xCA] = (byte) 0x90;
        ASCII2EBCDIC[0xCB] = (byte) 0x9A;
        ASCII2EBCDIC[0xCC] = (byte) 0x9B;
        ASCII2EBCDIC[0xCD] = (byte) 0x9C;
        ASCII2EBCDIC[0xCE] = (byte) 0x9D;
        ASCII2EBCDIC[0xCF] = (byte) 0x9E;
        ASCII2EBCDIC[0xD0] = (byte) 0x9F;
        ASCII2EBCDIC[0xD1] = (byte) 0xA0;
        ASCII2EBCDIC[0xD2] = (byte) 0xAA;
        ASCII2EBCDIC[0xD3] = (byte) 0xAB;
        ASCII2EBCDIC[0xD4] = (byte) 0xAC;
        ASCII2EBCDIC[0xD5] = (byte) 0xAD;
        ASCII2EBCDIC[0xD6] = (byte) 0xAE;
        ASCII2EBCDIC[0xD7] = (byte) 0xAF;
        ASCII2EBCDIC[0xD8] = (byte) 0xB0;
        ASCII2EBCDIC[0xD9] = (byte) 0xB1;
        ASCII2EBCDIC[0xDA] = (byte) 0xB2;
        ASCII2EBCDIC[0xDB] = (byte) 0xB3;
        ASCII2EBCDIC[0xDC] = (byte) 0xB4;
        ASCII2EBCDIC[0xDD] = (byte) 0xB5;
        ASCII2EBCDIC[0xDE] = (byte) 0xB6;
        ASCII2EBCDIC[0xDF] = (byte) 0xB7;
        ASCII2EBCDIC[0xE0] = (byte) 0xB8;
        ASCII2EBCDIC[0xE1] = (byte) 0xB9;
        ASCII2EBCDIC[0xE2] = (byte) 0xBA;
        ASCII2EBCDIC[0xE3] = (byte) 0xBB;
        ASCII2EBCDIC[0xE4] = (byte) 0xBC;
        ASCII2EBCDIC[0xE5] = (byte) 0xBD;
        ASCII2EBCDIC[0xE6] = (byte) 0xBE;
        ASCII2EBCDIC[0xE7] = (byte) 0xBF;
        ASCII2EBCDIC[0xE8] = (byte) 0xCA;
        ASCII2EBCDIC[0xE9] = (byte) 0xCB;
        ASCII2EBCDIC[0xEA] = (byte) 0xCC;
        ASCII2EBCDIC[0xEB] = (byte) 0xCD;
        ASCII2EBCDIC[0xEC] = (byte) 0xCE;
        ASCII2EBCDIC[0xED] = (byte) 0xCF;
        ASCII2EBCDIC[0xEE] = (byte) 0xDA;
        ASCII2EBCDIC[0xEF] = (byte) 0xDB;
        ASCII2EBCDIC[0xF0] = (byte) 0xDC;
        ASCII2EBCDIC[0xF1] = (byte) 0xDD;
        ASCII2EBCDIC[0xF2] = (byte) 0xDE;
        ASCII2EBCDIC[0xF3] = (byte) 0xDF;
        ASCII2EBCDIC[0xF4] = (byte) 0xEA;
        ASCII2EBCDIC[0xF5] = (byte) 0xEB;
        ASCII2EBCDIC[0xF6] = (byte) 0xEC;
        ASCII2EBCDIC[0xF7] = (byte) 0xED;
        ASCII2EBCDIC[0xF8] = (byte) 0xEE;
        ASCII2EBCDIC[0xF9] = (byte) 0xEF;
        ASCII2EBCDIC[0xFA] = (byte) 0xFA;
        ASCII2EBCDIC[0xFB] = (byte) 0xFB;
        ASCII2EBCDIC[0xFC] = (byte) 0xFC;
        ASCII2EBCDIC[0xFD] = (byte) 0xFD;
        ASCII2EBCDIC[0xFE] = (byte) 0xFE;
        ASCII2EBCDIC[0xFF] = (byte) 0xFF;
        // Use the current table to create the reverse conversion table.
        for (int i = 0; i < 256; ++i) {
            EBCDIC2ASCII[ASCII2EBCDIC[i] & 0xFF] = (byte) i;
        }
    }

    private byte convertByteA2E(byte in) {
        return ASCII2EBCDIC[in & 0xFF];
    }

    public byte[] convertStrToEBCDIC(byte[] in) {
        if (in != null) {
            byte[] out = new byte[in.length];
            for (int i = 0; i < in.length; ++i) {
                out[i] = convertByteA2E(in[i]);
            }
            return out;
        }
        return null;
    }

    public String toHex(byte[] in) {
        StringBuffer sb = new StringBuffer(6 * in.length);
        if (in != null) {
            for (int i = 0; i < in.length; ++i) {
                sb.append("0x" + Integer.toHexString(in[i] & 0xFF)
                        + ((i == in.length - 1) ? "." : ", "));
            }
            return sb.toString();
        }

        return null;
    }

    public Byte asciiToEbcdic(Byte ascii) {
        return ASCII2EBCDIC[ascii & 0xFF];
    }
    public Byte ebcdicToAscii(Byte ebcdic) {
        return EBCDIC2ASCII[ebcdic & 0xFF];
    }
}
