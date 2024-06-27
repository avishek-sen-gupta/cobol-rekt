package org.smojol.common.vm.type;

import org.apache.commons.lang3.StringUtils;

public class LeftAdjuster implements DataFilter {
    private final int maxLength;
    private final String padChar;

    public LeftAdjuster(int maxLength, String padChar) {
        this.maxLength = maxLength;
        this.padChar = padChar;
    }

    public LeftAdjuster(int maxLength) {
        this(maxLength, " ");
    }

    public String filter(String s) {
        return s.length() > maxLength ? s.substring(s.length() - maxLength) : StringUtils.leftPad(s, maxLength, padChar);
    }

    @Override
    public int sizeInBytes() {
        return maxLength;
    }
}
