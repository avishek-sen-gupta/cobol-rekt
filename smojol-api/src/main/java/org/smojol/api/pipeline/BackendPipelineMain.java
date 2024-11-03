package org.smojol.api.pipeline;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.gson.Gson;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.jooq.DSLContext;
import org.jooq.SQLDialect;
import org.jooq.impl.DSL;
import org.smojol.api.IntermediateFormService;
import org.smojol.api.ProjectService;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.pseudocode.BasicBlock;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.transpiler.PruneUnreachableTask;
import org.smojol.common.transpiler.TranspilerFlowgraph;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import org.smojol.toolkit.analysis.task.transpiler.BuildTranspilerFlowgraphTask;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.smojol.toolkit.task.CommandLineAnalysisTask.BUILD_BASE_ANALYSIS;

public class BackendPipelineMain {
    public static void main(String[] args) throws IOException, InterruptedException, SQLException {
        String programName = "test-exp.cbl";
        Map<String, List<AnalysisTaskResult>> analysisResult = new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.MERMAID), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(BUILD_BASE_ANALYSIS, CommandLineAnalysisTask.BUILD_TRANSPILER_FLOWGRAPH), ImmutableList.of(programName));
        List<AnalysisTaskResult> results = analysisResult.get(programName);
        TranspilerFlowgraph transpilerFlowgraph = ((AnalysisTaskResultOK) results.get(1)).getDetail();
        Graph<BasicBlock<TranspilerInstruction>, DefaultEdge> blockGraph = transpilerFlowgraph.basicBlockFlowgraph();
        Graph<TranspilerInstruction, DefaultEdge> instructionFlowgraph = transpilerFlowgraph.instructionFlowgraph();
        PruneUnreachableTask.pruneUnreachableInstructions(transpilerFlowgraph);
        TranspilerNode tree = transpilerFlowgraph.transpilerTree();
        ImmutableMap<String, Set<?>> irCFGForDB = ImmutableMap.of("nodes", instructionFlowgraph.vertexSet(), "edges", instructionFlowgraph.edgeSet());
        Gson gson = BuildTranspilerFlowgraphTask.initGson();

        String url = System.getenv("DATABASE_URL");
        String user = System.getenv("DATABASE_USER");
        String password = System.getenv("DATABASE_PASSWORD");
        IntermediateFormService intermediateFormService = new IntermediateFormService(gson);
        ProjectService projectService = new ProjectService();
        try (Connection conn = DriverManager.getConnection(url, user, password)) {
            DSLContext using = DSL.using(conn, SQLDialect.SQLITE);
            String projectName = UUID.randomUUID().toString();
            long projectID = projectService.insertProject(projectName, using);
            long irAstID = intermediateFormService.insertIntermediateAST(tree, programName, projectID, using);
            long irCfgID = intermediateFormService.insertIntermediateCFG(irCFGForDB, programName, projectID, using);
            System.out.println(projectID);
            System.out.println(irAstID);
            System.out.println(irCfgID);
        }
        System.out.println("DONE");
    }


}
