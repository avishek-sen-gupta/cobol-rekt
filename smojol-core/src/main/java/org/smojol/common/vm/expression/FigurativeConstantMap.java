package org.smojol.common.vm.expression;

import java.util.logging.Logger;

public class FigurativeConstantMap {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(FigurativeConstantMap.class.getName());
    public String map(String figurativeConstant) {
        LOGGER.finer("Mapping Figurative Constant: " + figurativeConstant);
        if ("SPACE".equals(figurativeConstant) || "SPACES".equals(figurativeConstant)) return " ";
        else if ("QUOTE".equals(figurativeConstant) || "QUOTES".equals(figurativeConstant)) return "\"";
        else if ("ZERO".equals(figurativeConstant) || "ZEROES".equals(figurativeConstant) || "ZEROS".equals(figurativeConstant)) return "0";
        else if ("NULL".equals(figurativeConstant)) return "";
        // TODO: Define LOW-VALUES and HIGH-VALUES properly
        else if ("LOW-VALUES".equals(figurativeConstant)) return "0";
        else if ("LOW-VALUE".equals(figurativeConstant)) return "0";
        else if ("HIGH-VALUES".equals(figurativeConstant)) return "100";
        else if ("HIGH-VALUE".equals(figurativeConstant)) return "100";
        throw new UnsupportedOperationException("Unsupported figurative constant: " + figurativeConstant);
    }
}
