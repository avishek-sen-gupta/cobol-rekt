package org.smojol.toolkit.analysis.task.analysis;

import com.google.gson.annotations.Expose;
import lombok.Getter;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.CallTarget;
import org.smojol.common.ast.ExternalControlFlowNode;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.navigation.FlowNodeNavigator;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

public class ProgramDependenciesTask implements AnalysisTask {
    java.util.logging.Logger LOGGER = Logger.getLogger(ProgramDependenciesTask.class.getName());
    private final FlowNode root;
    private final CobolParser.ProcedureDivisionBodyContext rawAST;
    private final CobolDataStructure dataStructures;
    private final SmojolSymbolTable symbolTable;
    @Expose
    private final String programName;
    @Expose @Getter private final List<CallTarget> dependencies = new ArrayList<>();

    public ProgramDependenciesTask(FlowNode root, CobolParser.ProcedureDivisionBodyContext rawAST, CobolDataStructure dataStructures, SmojolSymbolTable symbolTable, String programName) {
        this.root = root;
        this.rawAST = rawAST;
        this.dataStructures = dataStructures;
        this.symbolTable = symbolTable;
        this.programName = programName;
    }

    public boolean isEmpty() {
        return dependencies.isEmpty();
    }

    @Override
    public AnalysisTaskResult run() {
        List<FlowNode> allExternalCallNodes = new FlowNodeNavigator(root).findAllByCondition(fn -> fn instanceof ExternalControlFlowNode);
//        TranspilerFlowgraph transpilerFlowgraph = new BuildTranspilerFlowgraphTask(rawAST, dataStructures, symbolTable, ImmutableList.of()).run();
//        TranspilerNode tree = transpilerFlowgraph.transpilerTree();
//        List<TranspilerNode> allExternalCalls = tree.findAllRecursive(n -> n instanceof CallTranspilerNode);

//        List<PseudocodeInstruction> instructions = new BuildPseudocodeTask(root, new UUIDProvider()).run();
//        List<PseudocodeInstruction> allTransfers = instructions.stream().filter(ins -> ins.isBody() && ins.getNode() instanceof ExternalControlFlowNode).toList();
//        List<CallTarget> callTargets = allTransfers.stream().map(t -> ((ExternalControlFlowNode) t.getNode()).callTarget().resolve(t, instructions)).toList();
        List<CallTarget> callTargets = allExternalCallNodes.stream().map(t -> ((ExternalControlFlowNode) t).callTarget()).toList();
        dependencies.addAll(callTargets);

        return AnalysisTaskResult.OK(CommandLineAnalysisTask.BUILD_PROGRAM_DEPENDENCIES, dependencies);
    }
}
