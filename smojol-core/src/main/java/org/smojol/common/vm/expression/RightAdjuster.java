package org.smojol.common.vm.expression;

import org.apache.commons.lang3.StringUtils;
import org.smojol.common.vm.type.DataFilter;

public class RightAdjuster implements DataFilter {
    private final int maxLength;
    private final String padChar;

    public RightAdjuster(int maxLength, String padChar) {
        this.maxLength = maxLength;
        this.padChar = padChar;
    }

    public RightAdjuster(int maxLength) {
        this(maxLength, " ");
    }

    @Override
    public String filter(String s) {
        return s.length() > maxLength ? s.substring(0, maxLength) : StringUtils.rightPad(s, maxLength, padChar);
    }

    @Override
    public int sizeInBytes() {
        return maxLength;
    }
}
