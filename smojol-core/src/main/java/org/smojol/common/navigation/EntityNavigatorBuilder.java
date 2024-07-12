package org.smojol.common.navigation;

import org.antlr.v4.runtime.ParserRuleContext;

public class EntityNavigatorBuilder {
    public CobolEntityNavigator navigator(ParserRuleContext fullProgramTree) {
        return new CobolEntityNavigator(fullProgramTree);
    }
}
