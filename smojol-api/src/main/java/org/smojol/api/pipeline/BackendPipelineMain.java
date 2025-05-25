package org.smojol.api.pipeline;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.gson.Gson;
import com.mojo.algorithms.domain.BasicBlock;
import com.mojo.algorithms.domain.*;
import com.mojo.algorithms.domain.NamespaceQualifier;
import com.mojo.algorithms.id.UUIDProvider;
import com.mojo.algorithms.task.*;
import com.mojo.algorithms.transpiler.*;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.jooq.DSLContext;
import org.smojol.api.IntermediateFormService;
import org.smojol.api.ProjectService;
import org.smojol.api.SourceService;
import org.smojol.api.database.DbContext;
import org.smojol.common.ast.CobolContextAugmentedTreeNode;
import org.smojol.common.dialect.LanguageDialect;
import com.mojo.algorithms.visualisation.FlowchartOutputFormat;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.toolkit.analysis.graph.graphml.SerialisableUnifiedModel;
import org.smojol.toolkit.analysis.pipeline.BaseAnalysisModel;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.BuildFlowchartMarkupTask;
import org.smojol.toolkit.analysis.task.analysis.BuildTranspilerFlowgraphTask;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import org.smojol.toolkit.analysis.task.analysis.UnifiedFlowModelTask;
import org.smojol.toolkit.intermediate.NodeSpecBuilder;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.task.T1_T2_IntervalAnalysisTask;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static com.mojo.algorithms.domain.GraphOperations.duplicateGraph;
import static com.mojo.algorithms.task.CommandLineAnalysisTask.BUILD_BASE_ANALYSIS;

public class BackendPipelineMain {
    public static void main(String[] args) throws IOException, InterruptedException, SQLException {
        String programName = "test-exp.cbl";
//        String programName = "simple-loop.cbl";
        UUIDProvider idProvider = new UUIDProvider();
        Map<String, List<AnalysisTaskResult>> analysisResult = new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.MERMAID, idProvider), idProvider, new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(BUILD_BASE_ANALYSIS, CommandLineAnalysisTask.BUILD_TRANSPILER_FLOWGRAPH), ImmutableList.of(programName));
        List<AnalysisTaskResult> results = analysisResult.get(programName);

        BaseAnalysisModel baseModel = ((AnalysisTaskResultOK) results.getFirst()).getDetail();
        SerialisableUnifiedModel unifiedModel = new UnifiedFlowModelTask(baseModel.flowRoot(), baseModel.dataStructures(), new NodeSpecBuilder(new NamespaceQualifier("TEST_NAMESPACE"))).run();

        String flowchartMarkup = new BuildFlowchartMarkupTask(baseModel.flowRoot()).run();
        TranspilerFlowgraph transpilerFlowgraph = ((AnalysisTaskResultOK) results.get(1)).getDetail();
        Graph<BasicBlock<TranspilerInstruction>, DefaultEdge> blockGraph = transpilerFlowgraph.basicBlockFlowgraph();
        Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph = transpilerFlowgraph.instructionFlowgraph();
        PruneUnreachableTask.pruneUnreachableInstructions(transpilerFlowgraph);
        TranspilerNode tree = transpilerFlowgraph.transpilerTree();
        Pair<Set<NaturalLoopBody<TranspilerInstruction>>, Set<NaturalLoopBody<TranspilerInstruction>>> loopBodies = new LoopBodyDetectionTask<>(transpilerFlowgraph.instructions().getFirst(),
                transpilerFlowgraph.instructionFlowgraph(), DefaultEdge.class, CloneEdgeOperation::cloneEdge).run();
        Set<NaturalLoopBody<TranspilerInstruction>> reducibleLoopBodies = loopBodies.getLeft();
        Set<NaturalLoopBody<TranspilerInstruction>> irreducibleLoopBodies = loopBodies.getRight();
        System.out.println("Reducible loop bodies = " + reducibleLoopBodies.size());
        System.out.println("irreducible loop bodies = " + irreducibleLoopBodies.size());
        AnalysisTaskResultOK reducibilityTestResult = (AnalysisTaskResultOK) new T1_T2_IntervalAnalysisTask<>(duplicateGraph(instructionFlowgraph, DefaultEdge.class, e -> new DefaultEdge()), n -> n == transpilerFlowgraph.instructions().getFirst(),
                (from, to) -> new DefaultEdge()).run();
        FlowgraphReductionResult<TranspilerInstruction, DefaultEdge> reductionResult = reducibilityTestResult.getDetail();
        System.out.println("Was reducible = " + reductionResult.isReducible());
        reducibleLoopBodies.forEach(loop -> {
            System.out.println("-----------------------------");
            System.out.println(String.join(",", loop.loopNodes().stream().map(TranspilerInstruction::id).toList()));
            System.out.println("-----------------------------");
            System.out.println(new Gson().toJson(loop));
        });

        String url = System.getenv("DATABASE_URL");
        String user = System.getenv("DATABASE_USER");
        String password = System.getenv("DATABASE_PASSWORD");

        DbContext dbContext = new DbContext(url, user, password);
        long projectID = insertIntoDB(transpilerFlowgraph, reducibleLoopBodies, programName, reductionResult, unifiedModel, flowchartMarkup, dbContext, baseModel.serialisableAST());
        List<JumpTranspilerNode> gotos = tree.allOfType(JumpTranspilerNode.class);
        List<JumpTranspilerNode> demoGotos = gotos.stream().filter(g -> g.getStart() instanceof NamedLocationNode).toList();
        TreeSmith treeSmith = new TreeSmith(tree);
        Boolean allEliminated = demoGotos.stream().map(treeSmith::eliminateGoto).reduce(true, (a, b) -> a && b);
        System.out.println("Worked: " + allEliminated);
        insertRestructuredAST(tree, programName, projectID, dbContext);
    }

    private static void insertRestructuredAST(TranspilerNode tree, String programName, long projectID, DbContext dbContext) throws SQLException {
        Gson gson = BuildTranspilerFlowgraphTask.initGson();
        IntermediateFormService intermediateFormService = new IntermediateFormService(gson);
        dbContext.execute(using -> intermediateFormService.insertIntermediateAST(tree, programName + " [Refactored]", projectID, using));
    }

    private static long insertIntoDB(TranspilerFlowgraph transpilerFlowgraph, Set<NaturalLoopBody<TranspilerInstruction>> reducibleLoopBodies, String programName, FlowgraphReductionResult<TranspilerInstruction, DefaultEdge> reductionResult, SerialisableUnifiedModel unifiedModel, String flowchartMarkup, DbContext dbContext, CobolContextAugmentedTreeNode serialisableAST) throws SQLException {
        Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph = transpilerFlowgraph.instructionFlowgraph();
        TranspilerNode tree = transpilerFlowgraph.transpilerTree();
        ImmutableMap<String, Set<?>> irCFGForDB = ImmutableMap.of("nodes", instructionFlowgraph.vertexSet(), "edges", instructionFlowgraph.edgeSet());
        Gson gson = BuildTranspilerFlowgraphTask.initGson();

        IntermediateFormService intermediateFormService = new IntermediateFormService(gson);
        ProjectService projectService = new ProjectService();
        SourceService sourceService = new SourceService(gson);

        long pID = dbContext.execute(using -> {
            String projectName = UUID.randomUUID().toString();
            long projectID = projectService.insertProject(projectName, using);
            insertIntermediateArtifacts(reducibleLoopBodies, programName, reductionResult, using, intermediateFormService, tree, projectID, irCFGForDB);
            insertSourceArtifacts(unifiedModel, programName, projectID, sourceService, flowchartMarkup, using, serialisableAST);
            return projectID;
        });

        System.out.println("DONE, project ID = " + pID);
        return pID;
    }

    private static void insertSourceArtifacts(SerialisableUnifiedModel unifiedModel, String programName, long projectID, SourceService sourceService, String flowchartMarkup, DSLContext using, CobolContextAugmentedTreeNode serialisableAST) {
        long unifiedModelID = sourceService.insertUnifiedModel(unifiedModel, programName, projectID, using);
        long flowchartID = sourceService.insertFlowchart(flowchartMarkup, programName, projectID, "PROCEDURE_DIVISION", using);
        long rawASTID = sourceService.insertRawAST(serialisableAST, programName, projectID, using);
    }

    private static void insertIntermediateArtifacts(Set<NaturalLoopBody<TranspilerInstruction>> reducibleLoopBodies, String programName, FlowgraphReductionResult<TranspilerInstruction, DefaultEdge> reductionResult, DSLContext using, IntermediateFormService intermediateFormService, TranspilerNode tree, long projectID, ImmutableMap<String, Set<?>> irCFGForDB) {
        long irAstID = intermediateFormService.insertIntermediateAST(tree, programName, projectID, using);
        long irCfgID = intermediateFormService.insertIntermediateCFG(irCFGForDB, programName, projectID, using);
        List<Long> loopBodyIDs = intermediateFormService.insertLoopBody(reducibleLoopBodies, irCfgID, using);
        intermediateFormService.insertT1T2AnalysisResult(reductionResult, irCfgID, using);

        loopBodyIDs.forEach(lb -> System.out.println("Loop body ID = " + lb));
        System.out.println(projectID);
        System.out.println(irAstID);
        System.out.println(irCfgID);
    }
}
