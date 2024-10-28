package org.smojol.toolkit.analysis.task.analysis;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.common.id.IdProvider;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.pseudocode.SymbolReferenceBuilder;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.pipeline.BaseAnalysisResult;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;

import java.io.IOException;

public class BuildSeedModelTask implements AnalysisTask {
    private final ParsePipeline pipeline;
    private final IdProvider idProvider;

    public BuildSeedModelTask(ParsePipeline pipeline, IdProvider idProvider) {
        this.pipeline = pipeline;
        this.idProvider = idProvider;
    }

    @Override
    public AnalysisTaskResult run() {
        try {
            CobolEntityNavigator navigator = pipeline.parse();
            CobolParser.ProcedureDivisionBodyContext rawAST = navigator.procedureDivisionBody(navigator.getRoot());
            CobolDataStructure dataStructures = pipeline.getDataStructures();
            FlowchartBuilder flowcharter = pipeline.flowcharter();
            SmojolSymbolTable symbolTable = new SmojolSymbolTable(dataStructures, new SymbolReferenceBuilder(idProvider));
            flowcharter.buildFlowAST(rawAST).buildControlFlow().buildOverlay();
            FlowNode flowRoot = flowcharter.getRoot();
            flowRoot.resolve(symbolTable, dataStructures);
            return AnalysisTaskResult.OK("BUILD_SEED_MODEL", new BaseAnalysisResult(navigator, rawAST, dataStructures,
                    flowcharter, symbolTable, flowRoot));
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, "BUILD_SEED_MODEL");
        }
    }
}
