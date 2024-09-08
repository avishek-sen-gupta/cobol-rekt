package org.smojol.toolkit.analysis.graph;

import com.google.common.collect.ImmutableList;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.ConditionVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.*;

import java.util.List;
import java.util.logging.Logger;

public class DataDependencyPairComputer {
    private static final Logger logger = Logger.getLogger(DataDependencyPairComputer.class.getName());
    // TODO: This can be rewritten bottom-up by finding all references, and then working our way up to a statement or a sentence
    public static Pair<List<CobolDataStructure>, List<CobolDataStructure>> dependencyPairs(FlowNode node, CobolDataStructure dataRoot) {
        if (node.type() != FlowNodeType.MOVE
                && node.type() != FlowNodeType.COMPUTE
                && node.type() != FlowNodeType.ADD
                && node.type() != FlowNodeType.SUBTRACT
                && node.type() != FlowNodeType.MULTIPLY
                && node.type() != FlowNodeType.DIVIDE
                && node.type() != FlowNodeType.IF_BRANCH
        ) return ImmutablePair.of(ImmutableList.of(), ImmutableList.of());

        if (node.type() == FlowNodeType.IF_BRANCH) {
            IfFlowNode ifNode = (IfFlowNode) node;
            CobolParser.ConditionContext condition = ifNode.getCondition();
            logger.finer("Condition is " + condition.getText());
            ConditionVisitor visitor = new ConditionVisitor(dataRoot);
            condition.accept(visitor);
            CobolExpression expression = visitor.getExpression();
            List<CobolDataStructure> conditionStructures = staticExpressionsFromSingle(expression, dataRoot);
            return ImmutablePair.of(conditionStructures, ImmutableList.of());
        }
        if (node.type() == FlowNodeType.MOVE) {
            MoveFlowNode move = (MoveFlowNode) node;
            List<CobolDataStructure> froms = staticExpressionsFromSingle(move.getFromExpression(), dataRoot);
            List<CobolDataStructure> tos = staticExpressionsFromMany(move.getToExpressions(), dataRoot);
            return ImmutablePair.of(froms, tos);
        } else if (node.type() == FlowNodeType.COMPUTE) {
            ComputeFlowNode compute = (ComputeFlowNode) node;
            List<CobolDataStructure> froms = staticExpressionsFromSingle(compute.getRhsExpression(), dataRoot);
            List<CobolDataStructure> tos = staticExpressionsFromMany(compute.getDestinationExpressions(), dataRoot);
            return ImmutablePair.of(froms, tos);
        } else if (node.type() == FlowNodeType.ADD) {
            AddFlowNode add = (AddFlowNode) node;
            List<CobolDataStructure> froms = staticExpressionsFromMany(add.getToExpressions(), dataRoot);
            List<CobolDataStructure> tos = staticExpressionsFromMany(add.getFromExpressions(), dataRoot);
            return ImmutablePair.of(froms, tos);
        } else if (node.type() == FlowNodeType.SUBTRACT) {
            SubtractFlowNode subtract = (SubtractFlowNode) node;
            List<CobolDataStructure> minuends = staticExpressionsFromMany(subtract.getMinuendExpressions(), dataRoot);
            List<CobolDataStructure> subtrahends = staticExpressionsFromMany(subtract.getSubtrahendExpressions(), dataRoot);
            return ImmutablePair.of(subtrahends, minuends);
        } else if (node.type() == FlowNodeType.MULTIPLY) {
            MultiplyFlowNode multiply = (MultiplyFlowNode) node;
            List<CobolDataStructure> lhses = staticExpressionsFromSingle(multiply.getLhsExpression(), dataRoot);
            List<CobolDataStructure> rhses = staticExpressionsFromMany(multiply.getRhsExpressions(), dataRoot);
            return ImmutablePair.of(lhses, rhses);
        } else if (node.type() == FlowNodeType.DIVIDE) {
            DivideFlowNode divide = (DivideFlowNode) node;
            List<CobolDataStructure> dividends = staticExpressionsFromMany(divide.getDividendExpressions(), dataRoot);
            List<CobolDataStructure> divisors = staticExpressionsFromSingle(divide.getDivisorExpression(), dataRoot);
            return ImmutablePair.of(divisors, dividends);
        }
        return ImmutablePair.nullPair();
    }

    private static List<CobolDataStructure> staticExpressionsFromMany(List<CobolExpression> expressions, CobolDataStructure dataRoot) {
        return expressions.stream().flatMap(minuend -> staticExpressionsFromSingle(minuend, dataRoot).stream()).toList();
    }

    private static List<CobolDataStructure> staticExpressionsFromSingle(CobolExpression expression, CobolDataStructure dataRoot) {
        StaticExpressionCollector expressionCollector = new StaticExpressionCollector(dataRoot);
        expression.accept(expressionCollector);
        return expressionCollector.structures();
    }
}
