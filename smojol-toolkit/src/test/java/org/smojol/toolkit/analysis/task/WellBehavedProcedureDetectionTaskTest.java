package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.junit.jupiter.api.Test;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;
import org.smojol.toolkit.analysis.task.transpiler.WellBehavedProcedureDetectionTask;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.smojol.common.transpiler.TreeMatcher.*;

public class WellBehavedProcedureDetectionTaskTest {
    @Test
    public void canClassifyProcedureAsWellBehavedIfItHasProcedureCallToAnotherWellBehavedLeaf() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("abcd"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        TranspilerNode callProc = new JumpTranspilerNode(new NamedLocationNode("SEC-B"), new NamedLocationNode("SEC-B"));
        IfTranspilerNode ifStmt = new IfTranspilerNode(condition, new TranspilerCodeBlockNode(ImmutableList.of(callProc, set("abcd", 12))));
        TranspilerNode sectionA = new LabelledTranspilerCodeBlockNode("SEC-A", ImmutableList.of(ifStmt, set1, set2), ImmutableMap.of("type", FlowNodeType.SECTION));
        TranspilerNode sectionB = new LabelledTranspilerCodeBlockNode("SEC-B", ImmutableList.of(set1, set2), ImmutableMap.of("type", FlowNodeType.SECTION));
        TranspilerCodeBlockNode program = new TranspilerCodeBlockNode(ImmutableList.of(sectionA, sectionB));
        block_(
                labelledBlock_(
                        if_(block_(
                                        jmp_(),
                                        set_()),
                                any_()),
                        set_(),
                        set_()
                ),
                labelledBlock_(
                        set_(),
                        set_()
                )
        ).verify(program);
        Set<TranspilerNode> wellBehavedProcedures = new WellBehavedProcedureDetectionTask(program).run();
        assertEquals(ImmutableSet.of(sectionA, sectionB), wellBehavedProcedures);
    }
    @Test

    public void doesNotClassifyProcedureAsWellBehavedIfItHasGotoToAnotherWellBehavedLeaf() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("abcd"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        TranspilerNode gotoSomeplace = new JumpTranspilerNode(new NamedLocationNode("SEC-B"));
        IfTranspilerNode ifStmt = new IfTranspilerNode(condition, new TranspilerCodeBlockNode(ImmutableList.of(gotoSomeplace, set("abcd", 12))));
        TranspilerNode sectionA = new LabelledTranspilerCodeBlockNode("SEC-A", ImmutableList.of(ifStmt, set1, set2), ImmutableMap.of("type", FlowNodeType.SECTION));
        TranspilerNode sectionB = new LabelledTranspilerCodeBlockNode("SEC-B", ImmutableList.of(set1, set2), ImmutableMap.of("type", FlowNodeType.SECTION));
        TranspilerCodeBlockNode program = new TranspilerCodeBlockNode(ImmutableList.of(sectionA, sectionB));
        block_(
                labelledBlock_(
                        if_(block_(
                                        jmp_(),
                                        set_()),
                                any_()),
                        set_(),
                        set_()
                ),
                labelledBlock_(
                        set_(),
                        set_()
                )
        ).verify(program);
        Set<TranspilerNode> wellBehavedProcedures = new WellBehavedProcedureDetectionTask(program).run();
        assertEquals(ImmutableSet.of(sectionB), wellBehavedProcedures);
    }

    private static SetTranspilerNode set(String variable, int value) {
        return new SetTranspilerNode(new SymbolReferenceNode(variable), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(value)));
    }
}
