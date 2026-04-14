package org.smojol.toolkit.analysis.task;

import com.mojo.algorithms.domain.FlowNodeType;
import org.junit.jupiter.api.Test;
import org.smojol.common.ast.SerialisableASTFlowNode;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.AnalysisTaskResultOK;
import com.mojo.algorithms.task.CommandLineAnalysisTask;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Exercises all grammar constructs that changed between COBOL LSP 2.2.0 and 2.4.3.
 * See COBOL-LSP-INTEGRATION.md Section 4 for the migration patterns tested here.
 *
 * Grammar constructs covered:
 * 1. sectionOrParagraph (replaces procedureSection) - numeric and named sections
 * 2. tableCall argument wrapper - multi-dimensional array subscripts
 * 3. performUntilCondition intermediate rule - PERFORM UNTIL
 * 4. functionReference intermediate rule - FUNCTION LENGTH
 * 5. pictureString singular accessor - PIC clauses
 * 6. CobolParser (merged from CobolDataDivisionParser) - data division parsing
 */
class GrammarMigrationCoverageTest {

    @Test
    void flowASTIsBuiltSuccessfullyForAllMigratedConstructs() throws IOException {
        SerialisableASTFlowNode root = buildFlowAST();
        assertNotNull(root, "Flow AST root must not be null");
        assertFalse(root.getChildren().isEmpty(), "Flow AST must have children");
    }

    @Test
    void sectionNamesAreExtractedCorrectlyIncludingNumericSections() throws IOException {
        SerialisableASTFlowNode root = buildFlowAST();

        List<String> sectionNames = collectSectionNames(root);
        assertTrue(sectionNames.contains("100"),
                "Numeric section '100' must be found — exercises integerLiteral(0) extraction from SectionOrParagraphContext");
        assertTrue(sectionNames.contains("PROCESS-SECTION"),
                "Named section 'PROCESS-SECTION' must be found — exercises cobolWord() extraction from SectionOrParagraphContext");
        assertTrue(sectionNames.contains("FINAL-SECTION"),
                "Named section 'FINAL-SECTION' must be found");
        assertEquals(3, sectionNames.size(),
                "Exactly 3 sections expected — verifies SECTION() != null discriminator filters out paragraphs");
    }

    private SerialisableASTFlowNode buildFlowAST() throws IOException {
        AnalysisTaskResult taskResult = new TestTaskRunner("grammar-constructs.cbl", "test-code/grammar-coverage")
                .runTask(CommandLineAnalysisTask.WRITE_FLOW_AST);
        assertTrue(taskResult.isSuccess(), "Flow AST build must succeed for grammar-constructs.cbl");
        return ((AnalysisTaskResultOK) taskResult).getDetail();
    }

    private List<String> collectSectionNames(SerialisableASTFlowNode node) {
        List<String> result = new ArrayList<>();
        if (node.getType() == FlowNodeType.SECTION) {
            result.add(node.getName());
        }
        for (SerialisableASTFlowNode child : node.getChildren()) {
            result.addAll(collectSectionNames(child));
        }
        return result;
    }
}
