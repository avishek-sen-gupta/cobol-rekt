package org.smojol.common.flowchart;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.util.function.Function;

@Data
@AllArgsConstructor
public class VisitContext {
    public static boolean ALWAYS_VISIT(VisitContext ctx) {
        return true;
    }

    public static Function<VisitContext, Boolean> VISIT_UPTO_LEVEL(int maxLevel) {
        return (VisitContext ctx) -> maxLevel == -1 || ctx.getLevel() <= maxLevel;
    }

    private int level;

    public VisitContext oneLower() {
        return new VisitContext(level + 1);
    }
}
