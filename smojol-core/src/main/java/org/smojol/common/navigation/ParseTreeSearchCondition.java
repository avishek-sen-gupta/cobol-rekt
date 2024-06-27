package org.smojol.common.navigation;

import org.antlr.v4.runtime.tree.ParseTree;

public interface ParseTreeSearchCondition {
    boolean apply(ParseTree tree);
}
