package org.smojol.common.vm.type;

import org.smojol.common.vm.expression.RightAdjuster;

public class DecimalPointAligner implements DataFilter {
    private final int numIntegerDigits;
    private final int numFractionalDigits;

    public DecimalPointAligner(int numIntegerDigits, int numFractionalDigits) {
        this.numIntegerDigits = numIntegerDigits;
        this.numFractionalDigits = numFractionalDigits;
    }

    public String filter(String s) {
        int decimalPointPosition = s.indexOf(".");
        String lhs = decimalPointPosition != -1 ? s.substring(0, decimalPointPosition) : s;
        String rhs = decimalPointPosition != -1 ? s.substring(decimalPointPosition + 1) : "";
        return new LeftAdjuster(numIntegerDigits, ZonedDecimalDataTypeSpec.CHARACTER_ZERO).filter(lhs) + new RightAdjuster(numFractionalDigits, ZonedDecimalDataTypeSpec.CHARACTER_ZERO).filter(rhs);
    }

//    @Override
//    public String filter(String s, String padChar) {
//        int decimalPointPosition = s.indexOf(".");
//        String lhs = decimalPointPosition != -1 ? s.substring(0, decimalPointPosition) : s;
//        String rhs = decimalPointPosition != -1 ? s.substring(decimalPointPosition + 1) : "";
//        return new LeftAdjuster(numIntegerDigits, CHARACTER_ZERO).filter(lhs, padChar) + new RightAdjuster(numFractionalDigits, CHARACTER_ZERO).filter(rhs, padChar);
//    }

    @Override
    public int sizeInBytes() {
        return numIntegerDigits + numFractionalDigits;
    }
}
