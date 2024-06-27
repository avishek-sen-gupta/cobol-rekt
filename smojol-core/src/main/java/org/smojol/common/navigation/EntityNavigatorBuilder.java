package org.smojol.common.navigation;

import org.antlr.v4.runtime.ParserRuleContext;

public interface EntityNavigatorBuilder {
    CobolEntityNavigator navigator(ParserRuleContext fullProgramTree);
}
