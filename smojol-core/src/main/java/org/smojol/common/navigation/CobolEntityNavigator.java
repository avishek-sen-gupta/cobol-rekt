package org.smojol.common.navigation;

import org.antlr.v4.runtime.tree.ParseTree;

import java.util.List;

public interface CobolEntityNavigator {
    ParseTree target(String procedureName);
    ParseTree procedureBodyRoot();
    ParseTree dataDivisionBodyRoot();
    ParseTree findByCondition(ParseTree searchRoot, ParseTreeSearchCondition c, int maxLevel);
    ParseTree findByCondition(ParseTree searchRoot, ParseTreeSearchCondition c);
    List<ParseTree> statementsContaining(String symbol, ParseTree scope);
    List<ParseTree> findAllByCondition(ParseTreeSearchCondition c, ParseTree scope);
    List<ParseTree> findAllByCondition(ParseTreeSearchCondition c, ParseTree scope, int maxLevel);
    void buildDialectNodeRepository();
    String dialectText(String marker);
}
