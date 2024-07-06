package org.smojol.analysis.visualisation;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import hu.webarticum.treeprinter.printer.listing.ListingTreePrinter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;
import org.smojol.common.flowchart.CobolContextAugmentedTreeNode;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

/**
 * Draws IDMS AST
 */
public class IdmsTreeVisualiser {
    /**
     * Draws the tree
     *
     * @param startRuleContext
     * @param idmsParseTreeOutputPath
     * @param outputTree
     */
    public void visualiseIdmsAST(IdmsParser.StartRuleContext startRuleContext, String idmsParseTreeOutputPath, boolean outputTree) {
        Gson gson = new GsonBuilder().excludeFieldsWithoutExposeAnnotation().setPrettyPrinting().create();
        List<IdmsParser.IdmsRulesContext> idmsRulesContexts = startRuleContext.idmsRules();
        List<CobolContextAugmentedTreeNode> allAstTrees = new ArrayList<>();

        for (IdmsParser.IdmsRulesContext ctx : idmsRulesContexts) {
            IdmsParser.IdmsSectionsContext idmsSectionsContext = ctx.idmsSections();
            IdmsParser.IdmsStatementsContext idmsStatementsContext = ctx.idmsStatements();
            if (idmsSectionsContext != null && outputTree) {
                allAstTrees.addAll(drawParseTrees(idmsSectionsContext.children));
            }
            if (idmsStatementsContext != null && outputTree) {
                allAstTrees.addAll(drawParseTrees(idmsStatementsContext.children));
            }
        }

        try {
            String s = gson.toJson(allAstTrees);
            PrintWriter out = new PrintWriter(idmsParseTreeOutputPath);
            out.println(s);
            out.close();
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Visualises the IDMS AST
     * @param startRuleContext
     * @param idmsParseTreeOutputPath
     */
    public void visualiseIdmsAST(IdmsParser.StartRuleContext startRuleContext, String idmsParseTreeOutputPath) {
        visualiseIdmsAST(startRuleContext, idmsParseTreeOutputPath, true);
    }

    // TODO: Inject correct navigator
    private List<CobolContextAugmentedTreeNode> drawParseTrees(List<ParseTree> parseRuleContext) {
        List<ParseTree> trees = parseRuleContext;
        List<CobolContextAugmentedTreeNode> astTrees = new ArrayList<>();
        for (ParseTree tree : trees) {
            CobolContextAugmentedTreeNode graphRoot = new CobolContextAugmentedTreeNode(tree, null);
            buildGraph(tree, graphRoot);
            astTrees.add(graphRoot);
            new ListingTreePrinter().print(graphRoot);
        }
        return astTrees;
    }

    // TODO: Inject correct navigator
    private void buildGraph(ParseTree astParentNode, CobolContextAugmentedTreeNode graphParentNode) {
        for (int i = 0; i <= astParentNode.getChildCount() - 1; ++i) {
            ParseTree astChildNode = astParentNode.getChild(i);
            CobolContextAugmentedTreeNode graphChildNode = new CobolContextAugmentedTreeNode(astChildNode, null);
            graphParentNode.addChild(graphChildNode);
            buildGraph(astChildNode, graphChildNode);
        }
        graphParentNode.freeze();
    }
}
