package org.smojol.toolkit.structure;

import org.junit.jupiter.api.Test;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.CobolDataType;
import org.smojol.toolkit.analysis.pipeline.BaseAnalysisModel;
import org.smojol.toolkit.analysis.task.TestTaskRunner;
import org.smojol.toolkit.interpreter.structure.DefaultFormat1DataStructureBuilder;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultError;
import org.smojol.toolkit.task.AnalysisTaskResultOK;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.fail;
import static org.smojol.toolkit.structure.DataStructureMatcher.*;

public class CobolDataStructureBuilderTest {
    @Test
    public void canBuildDataStructures() throws IOException {
        AnalysisTaskResult taskResult = new TestTaskRunner("data-structures.cbl", "test-code/structure")
                .runTask2(CommandLineAnalysisTask.DO_NOTHING, new DefaultFormat1DataStructureBuilder());
        BaseAnalysisModel model = switch (taskResult) {
            case AnalysisTaskResultError e -> fail(e.getException());
            case AnalysisTaskResultOK o -> o.getDetail();
        };
        CobolDataStructure dsRoot = model.dataStructures();
        root(
                string("EXCHANGE-PART-01"),
                table("SOME-ARRAY", 3,
                        string("SOME-ARRAY"),
                        string("SOME-ARRAY"),
                        string("SOME-ARRAY")
                ),
                group("SOME-PART",
                        string("SOME-PART-1"),
                        string("SOME-PART-2"),
                        number("INVOICE-AMOUNT"),
                        number("VENDOR-CORRECTION")
                ),
                group("SOME-GROUP",
                        table("LEVEL-10-A", 3,
                                string("LEVEL-10-A"),
                                string("LEVEL-10-A"),
                                string("LEVEL-10-A")
                        ),
                        table("LEVEL-10-B", 2,
                                group("LEVEL-10-B",
                                        table("LEVEL-20-B", 2,
                                                string("LEVEL-20-B"),
                                                string("LEVEL-20-B")
                                        )
                                ),
                                group("LEVEL-10-B",
                                        table("LEVEL-20-B", 2,
                                                string("LEVEL-20-B"),
                                                string("LEVEL-20-B")
                                        )
                                )
                        ),
                        group("AA", true,
                                number("AA1"),
                                number("AA2")
                        )
                ),
                string("SOMETHING"),
                table("SOMEFRACTION", 2,
                        number("SOMEFRACTION"),
                        number("SOMEFRACTION")),
                group("SOME-UNION-1", true,
                        number("UNION-CHILD-1"),
                        string("UNION-CHILD-2", true)),
                number("SOMETEXT"),
                string("REDEF-SOMETEXT", true),
                number("NUMERIC-SOMETEXT", true),
                number("CENTURY"),
                number("REDEF", true),
                number("SCALED"),
                number("RESULT"),
                string("CONDI"),
                string("SOMETHING-LINKAGE"),
                table("SOMEFRACTION-LINKAGE", 2,
                        number("SOMEFRACTION-LINKAGE"),
                        number("SOMEFRACTION-LINKAGE")
                ),
                static_("WHEN-COMPILED", CobolDataType.STRING)
        ).match(dsRoot).verify();
    }
}

