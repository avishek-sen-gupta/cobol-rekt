package org.smojol.common.flowchart;

public class ConsoleColors {
    public static final String ANSI_RESET = "\u001B[0m";
    public static final String ANSI_BLACK = "\u001B[30m";
    public static final String ANSI_RED = "\u001B[31m";
    public static final String ANSI_GREEN = "\u001B[32m";
    public static final String ANSI_YELLOW = "\u001B[33m";
    public static final String ANSI_BLUE = "\u001B[34m";
    public static final String ANSI_PURPLE = "\u001B[35m";
    public static final String ANSI_CYAN = "\u001B[36m";
    public static final String ANSI_WHITE = "\u001B[37m";

    public static String coloured(String s, String colourCode) {
        return String.format("%s%s%s", colourCode, s, ANSI_RESET);
    }

    public static String coloured(String s, int colourCode) {
        return String.format("%s%s%s", String.format("\u001B[38;5;%sm", colourCode), s, ANSI_RESET);
    }

    public static String coloured(String s, int foregroundColourCode, int backgroundColourCode) {
        return String.format("%s%s%s", String.format("\u001B[48;5;%sm\u001B[38;5;%sm", backgroundColourCode, foregroundColourCode), s, ANSI_RESET);
    }

    public static String green(String s) {
        return coloured(s, ANSI_GREEN);
    }

    public static String red(String s) {
        return coloured(s, ANSI_RED);
    }

    public static String yellow(String s) {
        return coloured(s, ANSI_YELLOW);
    }

    public static String blue(String s) {
        return coloured(s, ANSI_BLUE);
    }

    public static String purple(String s) {
        return coloured(s, ANSI_PURPLE);
    }

    public static String cyan(String s) {
        return coloured(s, ANSI_CYAN);
    }

    public static String white(String s) {
        return coloured(s, ANSI_WHITE);
    }
}
