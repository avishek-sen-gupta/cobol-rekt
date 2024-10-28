package org.smojol.toolkit.analysis.pipeline;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.structure.CobolDataStructure;

public record BaseAnalysisResult(CobolEntityNavigator navigator,
                                 CobolParser.ProcedureDivisionBodyContext rawAST,
                                 CobolDataStructure dataStructures,
                                 FlowchartBuilder flowcharter,
                                 SmojolSymbolTable symbolTable,
                                 FlowNode flowRoot) {
}
