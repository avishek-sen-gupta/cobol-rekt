package org.smojol.common;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import org.junit.jupiter.api.Test;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.smojol.common.TreeMatcher.*;

public class TreeFormatterTest {
    @Test
    public void canEncloseStatementRangeInNewIfScopeAutomaticallyGivenJumpIfNodeForForwardJump() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode set5 = set("NOP", 80);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("EFG"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        JumpIfTranspilerNode jumpTranspilerNode = new JumpIfTranspilerNode(new NamedLocationNode("SOME_BLOCK"), condition);
        TranspilerNode jumpDestinationBlock = new LabelledTranspilerCodeBlockNode("SOME_BLOCK", ImmutableList.of(set1, set2), ImmutableMap.of("type", FlowNodeType.PARAGRAPH));
        TranspilerNode program = new TranspilerCodeBlockNode(ImmutableList.of(jumpTranspilerNode, set3, set4, set5, jumpDestinationBlock));
        block_(
                jmpIf_(),
                set_(),
                set_(),
                set_(),
                labelledBlock_("SOME_BLOCK",
                        set_(),
                        set_()
                )
        ).verify(program);
        String formattedProgram = new TranspilerNodeFormatter().format(program);
        String expectedFormattedProgram = """
                {
                \tjump_if(eq(ref('EFG'), primitive(true)), loc(SOME_BLOCK))
                \tset(primitive(50.0), ref('PQR'))
                \tset(primitive(70.0), ref('KLM'))
                \tset(primitive(80.0), ref('NOP'))
                \tBLOCK [SOME_BLOCK] {
                \t\tset(primitive(30.0), ref('ABC'))
                \t\tset(primitive(40.0), ref('DEF'))
                \t}
                }""";
        assertEquals(expectedFormattedProgram, formattedProgram);
    }

    private static SetTranspilerNode set(String variable, int value) {
        return new SetTranspilerNode(new SymbolReferenceNode(variable), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(value)));
    }
}
