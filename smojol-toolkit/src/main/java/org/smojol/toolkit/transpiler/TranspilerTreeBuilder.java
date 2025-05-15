package org.smojol.toolkit.transpiler;

import com.mojo.algorithms.domain.TranspilerNode;
import com.mojo.algorithms.transpiler.*;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.*;
import org.smojol.toolkit.intermediate.SectionParagraphMap;

public class TranspilerTreeBuilder {
    public static TranspilerNode flowToTranspiler(FlowNode node, CobolDataStructure dataStructures, SectionParagraphMap sectionParagraphMap) {
        return switch (node) {
            case MoveFlowNode n -> SetTranspilerNodeBuilder.build(n, dataStructures);
            case IfFlowNode n -> IfTranspilerNodeBuilder.build(n, dataStructures, sectionParagraphMap);
            case GenericOnClauseFlowNode n -> new PlaceholderTranspilerNode(n.originalText());
            case SearchFlowNode n -> SearchWhenNodeTranslator.build(n, dataStructures, sectionParagraphMap);
            case EvaluateFlowNode n -> EvaluateNodeTranslator.build(n, dataStructures, sectionParagraphMap);
            case AddFlowNode n -> AddTranspilerNodeBuilder.build(n, dataStructures);
            case SubtractFlowNode n -> SubtractTranspilerNodeBuilder.build(n, dataStructures);
            case MultiplyFlowNode n -> MultiplyTranspilerNodeBuilder.build(n, dataStructures);
            case DivideFlowNode n -> DivideTranspilerNodeBuilder.build(n, dataStructures);
            case ComputeFlowNode n -> ComputeTranspilerNodeBuilder.build(n, dataStructures);
            case DisplayFlowNode n -> DisplayTranspilerNodeBuilder.build(n, dataStructures);
            case ConditionalStatementFlowNode n -> flowToTranspiler(n.getActualStatement(), dataStructures, sectionParagraphMap);
            case ProcedureDivisionBodyFlowNode n -> LabelledTranspilerCodeBlockNodeBuilder.build(n, dataStructures, sectionParagraphMap);
            case SectionFlowNode n -> LabelledTranspilerCodeBlockNodeBuilder.build(n, dataStructures, sectionParagraphMap);
            case ParagraphsFlowNode n -> TranspilerCodeBlockNodeBuilder.build(n, dataStructures, sectionParagraphMap);
            case SentenceFlowNode n -> TranspilerCodeBlockNodeBuilder.build(n, dataStructures, sectionParagraphMap);
            case ParagraphFlowNode n -> LabelledTranspilerCodeBlockNodeBuilder.build(n, dataStructures, sectionParagraphMap);
            case GoToFlowNode n -> JumpNodeBuilder.build(n, dataStructures);
            case PerformProcedureFlowNode n -> PerformProcedureNodeBuilder.build(n, dataStructures);
            case PerformInlineFlowNode n -> PerformProcedureNodeBuilder.build(n, dataStructures, sectionParagraphMap);
            case NextSentenceFlowNode n -> new JumpTranspilerNode(new NextLocationNode());
            case ExitFlowNode n -> new ExitTranspilerNode();
            case StopFlowNode n -> new JumpTranspilerNode(new ProgramTerminalLocationNode());
            case CallFlowNode c -> CallTranspilerNodeBuilder.build(c.callTarget());
            default -> new PlaceholderTranspilerNode(node.originalText());
        };
    }
}
