package org.smojol.common.format;

public class TextFormat {
    public static String truncated(String s, int truncationLimit) {
        return s.length() > truncationLimit ? s.substring(0, truncationLimit) : s;
    }
}
