package org.smojol.common.navigation;

import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.NodeText;
import org.smojol.common.idms.DialectContainerNode;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.logging.Logger;
import java.util.regex.Pattern;

public class CobolEntityNavigator {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(CobolEntityNavigator.class.getName());
    @Getter private final ParseTree root;
    private List<ParseTree> dialectNodes;
    private Map<String, String> symbolText;

    public CobolEntityNavigator(ParseTree root) {
        this.root = root;
    }

    public CobolParser.ProcedureDivisionBodyContext procedureDivisionBody(ParseTree tree) {
        return (CobolParser.ProcedureDivisionBodyContext) findByConditionRecursive(tree, n -> n instanceof CobolParser.ProcedureDivisionBodyContext, 1, -1);
    }

    public CobolParser.DataDivisionContext dataDivisionBody(ParseTree tree) {
        return (CobolParser.DataDivisionContext) findByConditionRecursive(tree, n -> n instanceof CobolParser.DataDivisionContext, 1, -1);
    }

    public ParseTree target(String procedureName) {
        return findTargetRecursive(procedureName, root);
    }

    public ParseTree findByCondition(ParseTree searchRoot, ParseTreeSearchCondition c, int maxLevel) {
        return findByConditionRecursive(searchRoot, c, 1, maxLevel);
    }

    public ParseTree findByCondition(ParseTree searchRoot, ParseTreeSearchCondition c) {
        return findByConditionRecursive(searchRoot, c, 1, -1);
    }

    public <T> T findByCondition(ParseTree searchRoot, Class<T> type) {
        ParseTree searchResult = findByConditionRecursive(searchRoot, n -> n.getClass() == type, 1, -1);
        if (searchResult == null) return null;
        return (T) searchResult;
    }

    public ParseTree findByCondition(ParseTreeSearchCondition c) {
        return findByCondition(root, c);
    }

    public List<ParseTree> findAllByCondition(ParseTreeSearchCondition c) {
        return this.findAllByCondition(c, root);
    }

    public List<ParseTree> findAllByCondition(ParseTreeSearchCondition c, ParseTree scope) {
        return this.findAllByCondition(c, scope, -1);
    }

    public List<ParseTree> findAllByCondition(ParseTreeSearchCondition c, ParseTree scope, int maxLevel) {
        List<ParseTree> trees = new ArrayList<>();
        findAllByConditionRecursive(scope, trees, c, 1, maxLevel);
        return trees;
    }

    public void buildDialectNodeRepository() {
        dialectNodes = findAllByCondition(t -> t.getClass() == CobolParser.DialectNodeFillerContext.class, root);
        Pattern dialectMarkerPattern = Pattern.compile("_DIALECT_ ([0-9]+)", Pattern.MULTILINE);

        symbolText = new HashMap<>();
        dialectNodes.forEach(n -> {
            LOGGER.info("Adding to repository dialect node " + n.getText());
            if (n.getChildCount() == 0) {
                LOGGER.info("WARNING: The following dialect node has no children: " + n.getText());
            }
            String markerID = "_DIALECT_ " + n.getChild(1).getText();
            ParseTree idmsContainer = findByCondition(n, c -> c.getClass() == DialectContainerNode.class, 1);
            String text = NodeText.originalText(idmsContainer.getChild(0), NodeText::PASSTHROUGH);
            symbolText.put(markerID, text);
        });
    }

    public String dialectText(String marker) {
        if (symbolText == null || symbolText.get(marker) == null) return marker;
        return symbolText.get(marker);
    }

    private ParseTree findByConditionRecursive(ParseTree currentNode, ParseTreeSearchCondition c, int level, int maxLevel) {
        if (c.apply(currentNode)) return currentNode;
        if (maxLevel != -1 && level > maxLevel)
            return null;
        for (int i = 0; i <= currentNode.getChildCount() - 1; i++) {
            ParseTree searchResult = findByConditionRecursive(currentNode.getChild(i), c, level + 1, maxLevel);
            if (searchResult != null) return searchResult;
        }

        return null;
    }

    private void findAllByConditionRecursive(ParseTree currentNode, List<ParseTree> matchedTrees, ParseTreeSearchCondition c, int level, int maxLevel) {
        if (c.apply(currentNode)) {
            matchedTrees.add(currentNode);
        }
        if (maxLevel != -1 && level > maxLevel) return;
        for (int i = 0; i <= currentNode.getChildCount() - 1; i++) {
            findAllByConditionRecursive(currentNode.getChild(i), matchedTrees, c, level + 1, maxLevel);
        }
    }

    public ParseTree findTargetRecursive(String procedureName, ParseTree currentNode) {
        if (currentNode.getClass() == CobolParser.ParagraphContext.class) {
            String name = ((CobolParser.ParagraphContext) currentNode).paragraphDefinitionName().getText();
            if (name.equals(procedureName)) return currentNode;
        } else if (currentNode.getClass() == CobolParser.ProcedureSectionContext.class) {
            String name = ((CobolParser.ProcedureSectionContext) currentNode).procedureSectionHeader().sectionName().getText();
            if (name.equals(procedureName)) return currentNode;
        }
        for (int i = 0; i <= currentNode.getChildCount() - 1; i++) {
            ParseTree searchResult = findTargetRecursive(procedureName, currentNode.getChild(i));
            if (searchResult != null) return searchResult;
        }

        return null;
    }

    public static <T> T build(ParseTree tree, BiFunction<ParseTree, T, T> make, Function<ParseTree, Boolean> stopRecurseCondition) {
        return internalBuild(tree, null, make, stopRecurseCondition);
    }

    public static <T> T build(ParseTree tree, BiFunction<ParseTree, T, T> make) {
        return build(tree, make, CobolEntityNavigator.NEVER_STOP);
    }

    public static Function<ParseTree, Boolean> NEVER_STOP = n -> false;

    public static <T> T internalBuild(ParseTree tree, T parent, BiFunction<ParseTree, T, T> make, Function<ParseTree, Boolean> stopRecurseCondition) {
        T node = make.apply(tree, parent);
        if (stopRecurseCondition.apply(tree)) return node;
        for (int i = 0; i < tree.getChildCount(); i++) {
            T child = internalBuild(tree.getChild(i), node, make, stopRecurseCondition);
        }
        return node;
    }

    public ParseTree findNarrowestByCondition(ParseTreeSearchCondition c) {
        return findNarrowestByConditionRecursive(root, c);
    }

    private ParseTree findNarrowestByConditionRecursive(ParseTree currentNode, ParseTreeSearchCondition c) {
        if (!c.apply(currentNode)) return null;
        for (int j = 0; j <= currentNode.getChildCount() - 1; j++) {
            ParseTree searchResult = findNarrowestByConditionRecursive(currentNode.getChild(j), c);
            if (searchResult != null) return searchResult;
        }
        return currentNode;
    }
}
