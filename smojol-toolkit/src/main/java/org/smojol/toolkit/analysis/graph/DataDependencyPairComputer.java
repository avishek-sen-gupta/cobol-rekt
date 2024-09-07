package org.smojol.toolkit.analysis.graph;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.ConditionVisitor;
import org.smojol.common.vm.expression.CobolExpressionBuilder;
import org.smojol.common.vm.reference.ShallowReferenceBuilder;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.*;

import java.util.List;
import java.util.Map;

public class DataDependencyPairComputer {
    // TODO: This can be rewritten bottom-up by finding all references, and then working our way up to a statement or a sentence
    public static Map.Entry<List<CobolDataStructure>, List<CobolDataStructure>> dependencyPairs(FlowNode node, CobolDataStructure dataRoot) {
        if (node.type() != FlowNodeType.MOVE
                && node.type() != FlowNodeType.COMPUTE
                && node.type() != FlowNodeType.ADD
                && node.type() != FlowNodeType.SUBTRACT
                && node.type() != FlowNodeType.MULTIPLY
                && node.type() != FlowNodeType.DIVIDE
                && node.type() != FlowNodeType.IF_BRANCH
        ) return ImmutablePair.nullPair();

        ShallowReferenceBuilder referenceBuilder = new ShallowReferenceBuilder();
        if (node.type() == FlowNodeType.IF_BRANCH) {
            IfFlowNode ifNode = (IfFlowNode) node;
            CobolParser.ConditionContext condition = ifNode.getCondition();
            System.out.println("Condition is " + condition.getText());
            ConditionVisitor visitor = new ConditionVisitor(dataRoot);
            condition.accept(visitor);
            CobolExpression expression = visitor.getExpression();
            StaticExpressionCollector expressionCollector = new StaticExpressionCollector(dataRoot);
            expression.accept(expressionCollector);
            List<CobolDataStructure> conditionStructures = expressionCollector.structures();
            return ImmutablePair.of(conditionStructures, ImmutableList.of());
        }
        if (node.type() == FlowNodeType.MOVE) {
            MoveFlowNode move = (MoveFlowNode) node;
            List<CobolDataStructure> froms = ImmutableList.of(referenceBuilder.getShallowReference(move.getFrom(), dataRoot).resolve());
            List<CobolDataStructure> tos = move.getTos().stream().map(t -> referenceBuilder.getShallowReference(t, dataRoot).resolve()).toList();
            return ImmutablePair.of(froms, tos);
        } else if (node.type() == FlowNodeType.COMPUTE) {
            ComputeFlowNode compute = (ComputeFlowNode) node;
            CobolExpression expression = new CobolExpressionBuilder().arithmetic(compute.getRhs());
            StaticExpressionCollector expressionCollector = new StaticExpressionCollector(dataRoot);
            expression.accept(expressionCollector);
            List<CobolDataStructure> froms = expressionCollector.structures();
            List<CobolDataStructure> tos = compute.getDestinations().stream().map(d -> referenceBuilder.getShallowReference(d.generalIdentifier(), dataRoot).resolve()).toList();
            return ImmutablePair.of(froms, tos);
        } else if (node.type() == FlowNodeType.ADD) {
            AddFlowNode add = (AddFlowNode) node;
            List<CobolDataStructure> froms = add.getFroms().stream().map(f -> referenceBuilder.getShallowReference(f, dataRoot).resolve()).toList();
            List<CobolDataStructure> tos = add.getTos().stream().map(t -> referenceBuilder.getShallowReference(t, dataRoot).resolve()).toList();
            return ImmutablePair.of(froms, tos);
//            connect(froms, tos);
        } else if (node.type() == FlowNodeType.SUBTRACT) {
            SubtractFlowNode subtract = (SubtractFlowNode) node;
            List<CobolDataStructure> minuends = subtract.getLhses().stream().map(f -> referenceBuilder.getShallowReference(f, dataRoot).resolve()).toList();
            List<CobolDataStructure> subtrahends = subtract.getRhses().stream().map(t -> referenceBuilder.getShallowReference(t, dataRoot).resolve()).toList();
            return ImmutablePair.of(subtrahends, minuends);
        } else if (node.type() == FlowNodeType.MULTIPLY) {
            MultiplyFlowNode multiply = (MultiplyFlowNode) node;
            List<CobolDataStructure> lhses = ImmutableList.of(referenceBuilder.getShallowReference(multiply.getLhs(), dataRoot).resolve());
            List<CobolDataStructure> rhses = multiply.getRhses().stream().map(t -> referenceBuilder.getShallowReference(t.generalIdentifier(), dataRoot).resolve()).toList();
            return ImmutablePair.of(lhses, rhses);
        } else if (node.type() == FlowNodeType.DIVIDE) {
            DivideFlowNode divide = (DivideFlowNode) node;
            ParseTree executionContext = divide.getExecutionContext();
            CobolEntityNavigator navigator = new CobolEntityNavigator((ParserRuleContext) executionContext);
            List<ParseTree> allGeneralIdentifiers = navigator.findAllByCondition(n -> n instanceof CobolParser.GeneralIdentifierContext);
            List<CobolDataStructure> divisors = ImmutableList.of(referenceBuilder.getShallowReference(divide.getIntoDivisor(), dataRoot).resolve());
            List<CobolDataStructure> dividends = divide.getDividends().stream().map(t -> referenceBuilder.getShallowReference(t.generalIdentifier(), dataRoot).resolve()).toList();
            return ImmutablePair.of(divisors, dividends);
        }
        return ImmutablePair.nullPair();
    }
}
