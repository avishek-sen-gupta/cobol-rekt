package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.nio.json.JSONExporter;
import org.jooq.*;
import org.jooq.Record;
import org.jooq.impl.DSL;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.pseudocode.BasicBlock;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

import static org.jooq.impl.DSL.field;
import static org.jooq.impl.DSL.table;
import static org.smojol.toolkit.task.CommandLineAnalysisTask.BUILD_BASE_ANALYSIS;

public class BackendPipelineMain {
    public static void main(String[] args) throws IOException, InterruptedException, SQLException {
        String programName = "simple-if.cbl";
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
        TranspilerNode tree = transpilerFlowgraph.transpilerTree();
        StringWriter writer = new StringWriter();
        new JSONExporter<TranspilerInstruction, DefaultEdge>().exportGraph(instructionFlowgraph, writer);
        String s = writer.toString();

        String url = System.getenv("DATABASE_URL");
        String user = System.getenv("DATABASE_USER");
        String password = System.getenv("DATABASE_PASSWORD");
        try (Connection conn = DriverManager.getConnection(url, user, password)) {
            DSLContext using = DSL.using(conn, SQLDialect.MYSQL);
//            using.insertInto(table("PROJECT"))
//                    .columns(field("NAME"))
//                    .values("PROJECT-1").execute();
            List<Integer> where = using.select(field("ID")).from(table("PROJECT")).where(field("NAME").eq("PROJECT-1")).fetchInto(Integer.class);
//            JumpIfTranspilerNode transpilerNode = new JumpIfTranspilerNode(new NamedLocationNode("ABCD"), new EqualToNode(new ValueOfNode(new SymbolReferenceNode("A")), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(10))));
            String json = new Gson().toJson(tree);
//            JsonObject jsonObject = new Gson().fromJson(json, JsonObject.class);
            using.insertInto(table("IR_AST"))
                    .columns(field("PROGRAM_NAME"), field("PROJECT_ID"),
                            field("IR_AST"))
                    .values(programName, where.getFirst(), json).execute();
        }
//        List<String> transpiledCode = new StructuredProgramTheoremFormTranspilerTask(transpilerFlowgraph).run();
        System.out.println("DONE");
    }
}
