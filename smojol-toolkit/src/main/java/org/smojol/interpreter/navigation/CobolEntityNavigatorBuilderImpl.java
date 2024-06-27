package org.smojol.interpreter.navigation;

import org.antlr.v4.runtime.ParserRuleContext;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.EntityNavigatorBuilder;

public class CobolEntityNavigatorBuilderImpl implements EntityNavigatorBuilder {
    @Override
    public CobolEntityNavigator navigator(ParserRuleContext fullProgramTree) {
        return new CobolEntityNavigatorImpl(fullProgramTree);
    }
}
