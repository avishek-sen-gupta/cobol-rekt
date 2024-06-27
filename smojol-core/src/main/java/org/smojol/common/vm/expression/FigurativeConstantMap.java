package org.smojol.common.vm.expression;

public class FigurativeConstantMap {
    public String map(String figurativeConstant) {
        if ("SPACE".equals(figurativeConstant) || "SPACES".equals(figurativeConstant)) return " ";
        else if ("QUOTE".equals(figurativeConstant) || "QUOTES".equals(figurativeConstant)) return "\"";
        else if ("ZERO".equals(figurativeConstant) || "ZEROES".equals(figurativeConstant) || "ZEROS".equals(figurativeConstant)) return "0";
        else if ("NULL".equals(figurativeConstant)) return "";
        throw new UnsupportedOperationException("Unsupported figurative constant: " + figurativeConstant);
    }
}
