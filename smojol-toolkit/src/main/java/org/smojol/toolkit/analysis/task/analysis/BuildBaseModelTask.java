package org.smojol.toolkit.analysis.task.analysis;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.BuildSerialisableASTTask;
import org.smojol.common.ast.CobolContextAugmentedTreeNode;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import com.mojo.algorithms.id.IdProvider;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.pseudocode.SymbolReferenceBuilder;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.pipeline.BaseAnalysisModel;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.toolkit.ast.BuildFlowNodesTask;
import org.smojol.toolkit.ast.FlowNodeServiceImpl;
import com.mojo.algorithms.task.AnalysisTask;
import com.mojo.algorithms.task.AnalysisTaskResult;

import java.io.IOException;

public class BuildBaseModelTask implements AnalysisTask {
    private final ParsePipeline pipeline;
    private final IdProvider idProvider;

    public BuildBaseModelTask(ParsePipeline pipeline, IdProvider idProvider) {
        this.pipeline = pipeline;
        this.idProvider = idProvider;
    }

    @Override
    public AnalysisTaskResult run() {
        try {
            CobolEntityNavigator navigator = pipeline.parse();
            CobolParser.ProcedureDivisionBodyContext rawAST = navigator.procedureDivisionBody(navigator.getRoot());
            CobolContextAugmentedTreeNode serialisableAST = new BuildSerialisableASTTask().run(rawAST, navigator);
            CobolDataStructure dataStructures = pipeline.getDataStructures();
//            FlowchartBuilder flowcharter = pipeline.flowcharter();
            SmojolSymbolTable symbolTable = new SmojolSymbolTable(dataStructures, new SymbolReferenceBuilder(idProvider));
            FlowNodeService nodeService = new FlowNodeServiceImpl(navigator, dataStructures, idProvider);
            FlowNode flowRoot = new BuildFlowNodesTask(nodeService).run(rawAST);
//            flowcharter.buildFlowAST(rawAST).buildControlFlow().buildOverlay();
//            FlowNode flowRoot = flowcharter.getRoot();
            flowRoot.resolve(symbolTable, dataStructures);
            return AnalysisTaskResult.OK("BUILD_SEED_MODEL", new BaseAnalysisModel(navigator, rawAST, dataStructures,
                    symbolTable, flowRoot, serialisableAST));
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, "BUILD_SEED_MODEL");
        }
    }
}
