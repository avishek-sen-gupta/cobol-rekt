package org.smojol.toolkit.flowchart;

import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.attribute.Shape;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;

import java.util.HashMap;

public class FlowchartStylePreferences {
    private static final HashMap<FlowNodeType, GraphvizStyleScheme> schemes = new HashMap<>();
    static {
        schemes.put(FlowNodeType.PARAGRAPH_NAME, new GraphvizStyleScheme(Color.WHEAT, Color.BLACK, Shape.NONE));
        schemes.put(FlowNodeType.GENERIC_PROCESSING, new GraphvizStyleScheme(Color.WHEAT, Color.BLACK));
        schemes.put(FlowNodeType.ATOMIC, new GraphvizStyleScheme(Color.WHEAT, Color.BLACK, Shape.POINT));
        schemes.put(FlowNodeType.STOP, new GraphvizStyleScheme(Color.DEEPSKYBLUE, Color.WHITE, Shape.CIRCLE));
        schemes.put(FlowNodeType.EXIT, new GraphvizStyleScheme(Color.RED3, Color.WHITE, Shape.DOUBLE_CIRCLE));
        schemes.put(FlowNodeType.SYMBOL, new GraphvizStyleScheme(Color.WHEAT, Color.BLACK, Shape.M_CIRCLE));
        schemes.put(FlowNodeType.DISPLAY, new GraphvizStyleScheme(Color.BLACK, Color.GREEN1));
        schemes.put(FlowNodeType.GENERIC_STATEMENT, new GraphvizStyleScheme(Color.WHEAT, Color.BLACK, Shape.INV_HOUSE));
        schemes.put(FlowNodeType.MOVE, new GraphvizStyleScheme(Color.PURPLE4, Color.BLACK, Shape.NOTE));
        schemes.put(FlowNodeType.COMPUTE, new GraphvizStyleScheme(Color.PURPLE4, Color.BLACK, Shape.NOTE));
        schemes.put(FlowNodeType.ADD, new GraphvizStyleScheme(Color.PURPLE4, Color.BLACK, Shape.NOTE));
        schemes.put(FlowNodeType.SUBTRACT, new GraphvizStyleScheme(Color.PURPLE4, Color.BLACK, Shape.NOTE));
        schemes.put(FlowNodeType.MULTIPLY, new GraphvizStyleScheme(Color.PURPLE4, Color.BLACK, Shape.NOTE));
        schemes.put(FlowNodeType.PARAGRAPHS, new GraphvizStyleScheme(Color.WHEAT, Color.BLACK, Shape.POINT));
        schemes.put(FlowNodeType.SENTENCE, new GraphvizStyleScheme(Color.WHEAT, Color.BLACK, Shape.POINT));
        schemes.put(FlowNodeType.COMPOSITE, new GraphvizStyleScheme(Color.WHEAT, Color.BLACK));
        schemes.put(FlowNodeType.SECTION, new GraphvizStyleScheme(Color.DEEPSKYBLUE4, Color.WHITE));
        schemes.put(FlowNodeType.PROCEDURE_DIVISION_BODY, new GraphvizStyleScheme(Color.DEEPSKYBLUE4, Color.WHITE));
        schemes.put(FlowNodeType.PARAGRAPH, new GraphvizStyleScheme(Color.DARKSEAGREEN4, Color.WHITE));
        schemes.put(FlowNodeType.DIALECT, new GraphvizStyleScheme(Color.BLUE4, Color.WHITE, Shape.FOLDER));
        schemes.put(FlowNodeType.SECTION_HEADER, new GraphvizStyleScheme(Color.BLUE4, Color.WHITE, Shape.POINT));
        schemes.put(FlowNodeType.SEARCH, new GraphvizStyleScheme(Color.DARKGOLDENROD4, Color.WHITE, Shape.INV_HOUSE));
        schemes.put(FlowNodeType.SEARCH_WHEN, new GraphvizStyleScheme(Color.DEEPSKYBLUE4, Color.WHITE, Shape.NOTE));
        schemes.put(FlowNodeType.AT_END_PHRASE, new GraphvizStyleScheme(Color.INDIANRED, Color.WHITE, Shape.DOUBLE_CIRCLE));
        schemes.put(FlowNodeType.GOTO, new GraphvizStyleScheme(Color.DARKGREEN, Color.WHITE, Shape.INV_HOUSE));
        schemes.put(FlowNodeType.CONTROL_FLOW, new GraphvizStyleScheme(Color.DARKORANGE2, Color.WHITE, Shape.R_ARROW));
        schemes.put(FlowNodeType.CALL, new GraphvizStyleScheme(Color.DARKORANGE2, Color.WHEAT, Shape.R_ARROW));
        schemes.put(FlowNodeType.TRANSFER, new GraphvizStyleScheme(Color.DARKORCHID4, Color.WHEAT, Shape.R_ARROW));
        schemes.put(FlowNodeType.PERFORM, new GraphvizStyleScheme(Color.DARKVIOLET, Color.WHITE, Shape.CDS));
        schemes.put(FlowNodeType.PERFORM_TEST, new GraphvizStyleScheme(Color.WHITE, Color.BLACK, Shape.UNDERLINE));
        schemes.put(FlowNodeType.NEXT_SENTENCE, new GraphvizStyleScheme(Color.DARKSLATEGRAY4, Color.WHITE, Shape.R_ARROW));
        schemes.put(FlowNodeType.IF_BRANCH, new GraphvizStyleScheme(Color.CHOCOLATE4, Color.WHITE, Shape.DIAMOND));
        schemes.put(FlowNodeType.EVALUATE, new GraphvizStyleScheme(Color.RED4, Color.WHITE, Shape.DIAMOND));
        schemes.put(FlowNodeType.CONDITIONAL_STATEMENT, new GraphvizStyleScheme(Color.CHOCOLATE4, Color.WHITE, Shape.POINT));
        schemes.put(FlowNodeType.CONDITION_CLAUSE, new GraphvizStyleScheme(Color.CHOCOLATE4, Color.WHITE, Shape.POINT));
        schemes.put(FlowNodeType.ON_CLAUSE, new GraphvizStyleScheme(Color.PURPLE4, Color.WHITE, Shape.HEXAGON));
        schemes.put(FlowNodeType.IF_YES, new GraphvizStyleScheme(Color.DARKGREEN, Color.WHITE, Shape.DOUBLE_CIRCLE));
        schemes.put(FlowNodeType.IF_NO, new GraphvizStyleScheme(Color.RED, Color.WHITE, Shape.DOUBLE_CIRCLE));
        schemes.put(FlowNodeType.BIND_RUN_UNIT, new GraphvizStyleScheme(Color.GREEN, Color.WHITE, Shape.L_ARROW));
        schemes.put(FlowNodeType.FINISH, new GraphvizStyleScheme(Color.RED, Color.WHITE, Shape.L_ARROW));
        schemes.put(FlowNodeType.DIALECT_CONTAINER, new GraphvizStyleScheme(Color.PURPLE4, Color.WHITE, Shape.PARALLELOGRAM));
        schemes.put(FlowNodeType.ON_CLAUSE_ACTION, new GraphvizStyleScheme(Color.PURPLE4, Color.WHITE, Shape.BOX));
        schemes.put(FlowNodeType.DUMMY, new GraphvizStyleScheme(Color.DARKSLATEGRAY4, Color.WHITE));
    }

    public static GraphvizStyleScheme scheme(FlowNode node) {
        return schemes.get(node.type());
    }
}
