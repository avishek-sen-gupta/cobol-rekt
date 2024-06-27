package org.smojol.common.flowchart;

import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;
import hu.webarticum.treeprinter.SimpleTreeNode;
import hu.webarticum.treeprinter.TreeNode;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.TextSpan;

import java.text.MessageFormat;
import java.util.List;

/**
 *  Visualisation Tree Node that encapsulates the actual AST node
 */
public class CobolContextAugmentedTreeNode extends SimpleTreeNode {
    private final ParseTree astNode;

    @Expose
    @SerializedName("nodeType")
    private final String nodeType;
    @Expose
    @SerializedName("text")
    private final String originalText;

    @Expose
    @SerializedName("children")
    private List<TreeNode> childrenRef;

//    @Expose
    @SerializedName("span")
    private TextSpan span;
    private final CobolEntityNavigator navigator;

    public CobolContextAugmentedTreeNode(ParseTree astNode, CobolEntityNavigator navigator) {
        super(astNode.getClass().getSimpleName());
        this.astNode = astNode;
        this.nodeType = astNode.getClass().getSimpleName();
        this.navigator = navigator;
        this.originalText = withType(astNode, false);
        this.span = createSpan(astNode);
    }

    private TextSpan createSpan(ParseTree astNode) {
        if (!(astNode instanceof ParserRuleContext)) {
            TerminalNode terminalNode = (TerminalNode) astNode;
            return new TextSpan(terminalNode.getSymbol().getLine(), terminalNode.getSymbol().getLine(), terminalNode.getSymbol().getCharPositionInLine(), -1, terminalNode.getSymbol().getStartIndex(), terminalNode.getSymbol().getStopIndex());
        }
        ParserRuleContext context = (ParserRuleContext) astNode;
        Token start = context.getStart();
        Token stop = context.getStop();
        return new TextSpan(start.getLine(), stop.getLine(), start.getCharPositionInLine(), stop.getCharPositionInLine(), start.getStartIndex(), stop.getStopIndex());
    }
    @Override
    public String content() {
        String formattedExtent = MessageFormat.format("({0}])", span.content());
        return astNode.getClass().getSimpleName() + " / " + withType(astNode, true) + " " + formattedExtent;
    }

    private String withType(ParseTree astNode, boolean truncate) {
        String originalText = NodeText.originalText(astNode, navigator::dialectText);
        return truncate ? truncated(originalText) : originalText;
    }

    private String truncated(String text) {
        return text.length() > 50 ? text.substring(0, 50) + " ... (truncated)" : text;
    }

    /**
     * Creates a new reference to the children that will be used for serialisation to JSON
     */
    public void freeze() {
        this.childrenRef = super.children();
    }
}
