package org.smojol.common;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import org.junit.jupiter.api.Test;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;

import static org.smojol.common.TreeMatcher.*;

public class WellBehavedProcedureDetectionTaskTest {
    @Test
    public void canDetectWellBehavedProcedures() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("abcd"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        TranspilerNode gotoSomeplace = new JumpTranspilerNode(new NamedLocationNode("SEC-A"));
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
    }

    private static SetTranspilerNode set(String variable, int value) {
        return new SetTranspilerNode(new SymbolReferenceNode(variable), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(value)));
    }
}
