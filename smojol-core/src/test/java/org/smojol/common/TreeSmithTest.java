package org.smojol.common;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.mojo.algorithms.list.CarCdr;
import com.mojo.algorithms.transpiler.*;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Test;

import java.util.List;

import static com.mojo.algorithms.transpiler.TreeMatcher.*;
import static org.junit.jupiter.api.Assertions.*;

public class TreeSmithTest {
    @Test
    public void canEscapeArbitraryScopeUsingJumpIfForForwardJump() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("abcd"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        TranspilerNode gotoSomeplace = new JumpTranspilerNode(new NamedLocationNode("SOMEPLACE"));
        IfTranspilerNode ifStmt = new IfTranspilerNode(condition, new TranspilerCodeBlockNode(ImmutableList.of(gotoSomeplace, set("abcd", 12))));
        TranspilerCodeBlockNode program = new TranspilerCodeBlockNode(ImmutableList.of(ifStmt, set1, set2));
        block_(
                if_(block_(
                                jmp_(),
                                set_()),
                        any_()),
                set_(),
                set_()
        ).verify(program);
        TreeSmith treeOps = new TreeSmith(program);
        Pair<TranspilerNode, Boolean> escapeResult = treeOps.escapeScopeOnce(gotoSomeplace);
        assertTrue(escapeResult.getRight());
        assertNotSame(gotoSomeplace, escapeResult.getLeft());
        block_(
                if_(block_(
                                set_(),
                                if_(block_(
                                        set_()
                                ), any_())
                        ),
                        any_()),
                jmpIf_(),
                set_(),
                set_()
        ).verify(program);
    }

    @Test
    public void canAssertStructure() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("abcd"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        TranspilerNode gotoSomeplace = new JumpTranspilerNode(new NamedLocationNode("SOMEPLACE"));
        IfTranspilerNode ifStmt = new IfTranspilerNode(condition, new TranspilerCodeBlockNode(ImmutableList.of(gotoSomeplace, set("abcd", 12))));
        TranspilerCodeBlockNode blockNode = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2, ifStmt));
        block_(
                set_(),
                set_(),
                if_(block_(
                                jmp_(),
                                set_()),
                        any_())
        ).verify(blockNode);
    }

    @Test
    public void canEncloseStatementRangeInNewScope() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode program = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2, set3, set4));

        List<TranspilerNode> range = program.range(set2, set4);
        assertEquals(ImmutableList.of(set2, set3, set4), range);
        assertEquals(ImmutableList.of(set2, set3), CarCdr.init(range));
        TranspilerCodeBlockNode newScope = new TranspilerCodeBlockNode(CarCdr.init(range));
        assertEquals(ImmutableList.of(set2, set3), newScope.astChildren());
    }

    @Test
    public void canEncloseStatementRangeInNewScopeAutomaticallyGivenSimpleJumpNodeForBackJump() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        JumpTranspilerNode jumpTranspilerNode = new JumpTranspilerNode(new NamedLocationNode("SOME_BLOCK"));
        TranspilerNode jumpBlock = new LabelledTranspilerCodeBlockNode("SOME_BLOCK", ImmutableList.of(set1, set2), ImmutableMap.of("type", FlowNodeType.PARAGRAPH));
        TranspilerNode program = new TranspilerCodeBlockNode(ImmutableList.of(jumpBlock, set3, set4, jumpTranspilerNode));
        block_(
                labelledBlock_("SOME_BLOCK",
                        set_(),
                        set_()
                ),
                set_(),
                set_(),
                jmp_()
        ).verify(program);
        assertTrue(new TreeSmith(program).eliminateBackJump(jumpTranspilerNode));
        block_(loop_(block_(
                                labelledBlock_("SOME_BLOCK",
                                        set_(),
                                        set_()
                                ),
                                set_(),
                                set_(),
                                set_()
                        )
                )
        ).verify(program);
    }

    @Test
    public void canEncloseStatementRangeInNewScopeAutomaticallyGivenJumpIfNodeForBackJump() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("EFG"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        JumpIfTranspilerNode jumpTranspilerNode = new JumpIfTranspilerNode(new NamedLocationNode("SOME_BLOCK"), condition);
        TranspilerNode jumpBlock = new LabelledTranspilerCodeBlockNode("SOME_BLOCK", ImmutableList.of(set1, set2), ImmutableMap.of("type", FlowNodeType.PARAGRAPH));
        TranspilerNode program = new TranspilerCodeBlockNode(ImmutableList.of(jumpBlock, set3, set4, jumpTranspilerNode));
        block_(
                labelledBlock_("SOME_BLOCK",
                        set_(),
                        set_()
                ),
                set_(),
                set_(),
                jmpIf_()
        ).verify(program);
        assertTrue(new TreeSmith(program).eliminateBackJump(jumpTranspilerNode));
        block_(loop_(
                        block_(
                                labelledBlock_("SOME_BLOCK",
                                        set_(),
                                        set_()
                                ),
                                set_(),
                                set_()
                        )
                )
        ).verify(program);
    }

    @Test
    public void canEncloseStatementRangeInNewIfScopeAutomaticallyGivenSimpleJumpNodeForForwardJump() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode set5 = set("NOP", 80);
        JumpTranspilerNode jumpTranspilerNode = new JumpTranspilerNode(new NamedLocationNode("SOME_BLOCK"));
        TranspilerNode jumpDestinationBlock = new LabelledTranspilerCodeBlockNode("SOME_BLOCK", ImmutableList.of(set1, set2), ImmutableMap.of("type", FlowNodeType.PARAGRAPH));
        TranspilerNode program = new TranspilerCodeBlockNode(ImmutableList.of(jumpTranspilerNode, set3, set4, set5, jumpDestinationBlock));
        block_(
                jmp_(),
                set_(),
                set_(),
                set_(),
                labelledBlock_("SOME_BLOCK",
                        set_(),
                        set_()
                )
        ).verify(program);
        assertTrue(new TreeSmith(program).eliminateForwardJump(jumpTranspilerNode));
        block_(
                set_(),
                if_(
                        block_(
                                set_(),
                                set_(),
                                set_()
                        ),
                        any_()
                ),
                labelledBlock_("SOME_BLOCK",
                        set_(),
                        set_()
                )
        ).verify(program);
    }

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
        assertTrue(new TreeSmith(program).eliminateForwardJump(jumpTranspilerNode));
        block_(
                if_(
                        block_(
                                set_(),
                                set_(),
                                set_()
                        ),
                        any_()
                ),
                labelledBlock_("SOME_BLOCK",
                        set_(),
                        set_()
                )
        ).verify(program);
    }

    @Test
    public void canEscapeScopeOnceAutomaticallyToCorrectLevelGivenJumpIfNode() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode set5 = set("NOP", 80);
        TranspilerNode set6 = set("RST", 90);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("EFG"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        JumpIfTranspilerNode jumpTranspilerNode = new JumpIfTranspilerNode(new NamedLocationNode("SOME_BLOCK"), condition);
        TranspilerNode jumpDestinationBlock = new LabelledTranspilerCodeBlockNode("SOME_BLOCK", ImmutableList.of(set1, set2), ImmutableMap.of("type", FlowNodeType.PARAGRAPH));
        IfTranspilerNode ifStmt = new IfTranspilerNode(condition, new TranspilerCodeBlockNode(ImmutableList.of(new TranspilerCodeBlockNode(ImmutableList.of(jumpTranspilerNode, set6)), set("abcd", 12))));
        TranspilerNode program = new TranspilerCodeBlockNode(ImmutableList.of(ifStmt, set3, set4, set5, jumpDestinationBlock));
        block_(
                if_(block_(
                        block_(
                                jmpIf_(),
                                set_()
                        ),
                        set_()
                ), any_()),
                set_(),
                set_(),
                set_(),
                labelledBlock_("SOME_BLOCK",
                        set_(),
                        set_()
                )
        ).verify(program);

        TreeSmith treeSmith = new TreeSmith(program);
        Pair<TranspilerNode, Boolean> promotionResult = treeSmith.escapeScope(jumpTranspilerNode);
        assertTrue(promotionResult.getRight());
        block_(
                if_(block_(
                        block_(
                                set_(),
                                if_(block_(
                                        set_()
                                ), any_())
                        ),
                        set_(),
                        if_(block_(
                                        set_()
                                ), any_()
                        )
                ), any_()),
                jmpIf_(),
                set_(),
                set_(),
                set_(),
                labelledBlock_("SOME_BLOCK",
                        set_(),
                        set_()
                )
        ).verify(program);
    }

    @Test
    public void canEliminateForwardGotoByCorrectLevelPromotion() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode set5 = set("NOP", 80);
        TranspilerNode set6 = set("RST", 90);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("EFG"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        JumpIfTranspilerNode jumpTranspilerNode = new JumpIfTranspilerNode(new NamedLocationNode("SOME_BLOCK"), condition);
        TranspilerNode jumpDestinationBlock = new LabelledTranspilerCodeBlockNode("SOME_BLOCK", ImmutableList.of(set1, set2), ImmutableMap.of("type", FlowNodeType.PARAGRAPH));
        IfTranspilerNode ifStmt = new IfTranspilerNode(condition, new TranspilerCodeBlockNode(ImmutableList.of(new TranspilerCodeBlockNode(ImmutableList.of(jumpTranspilerNode, set6)), set("abcd", 12))));
        TranspilerNode program = new TranspilerCodeBlockNode(ImmutableList.of(ifStmt, set3, set4, set5, jumpDestinationBlock));
        block_(
                if_(block_(
                        block_(
                                jmpIf_(),
                                set_()
                        ),
                        set_()
                ), any_()),
                set_(),
                set_(),
                set_(),
                labelledBlock_("SOME_BLOCK",
                        set_(),
                        set_()
                )
        ).verify(program);

        TreeSmith treeSmith = new TreeSmith(program);
        boolean eliminatedGoto = treeSmith.eliminateGoto(jumpTranspilerNode);
        assertTrue(eliminatedGoto);

        block_(
                if_(block_(
                        block_(
                                set_(),
                                if_(block_(
                                        set_()
                                ), any_())
                        ),
                        set_(),
                        if_(block_(
                                        set_()
                                ), any_()
                        )
                ), any_()),
                if_(block_(
                                set_(),
                                set_(),
                                set_()
                        ), any_()
                ),
                labelledBlock_("SOME_BLOCK",
                        set_(),
                        set_()
                )
        ).verify(program);
    }

    @Test
    public void canEliminateBackwardGotoByCorrectLevelPromotion() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode set5 = set("NOP", 80);
        TranspilerNode set6 = set("RST", 90);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("EFG"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        JumpIfTranspilerNode jumpTranspilerNode = new JumpIfTranspilerNode(new NamedLocationNode("SOME_BLOCK"), condition);
        TranspilerNode jumpDestinationBlock = new LabelledTranspilerCodeBlockNode("SOME_BLOCK", ImmutableList.of(set1, set2), ImmutableMap.of("type", FlowNodeType.PARAGRAPH));
        IfTranspilerNode ifStmt = new IfTranspilerNode(condition, new TranspilerCodeBlockNode(ImmutableList.of(new TranspilerCodeBlockNode(ImmutableList.of(jumpTranspilerNode, set6)), set("abcd", 12))));
        TranspilerNode program = new TranspilerCodeBlockNode(ImmutableList.of(jumpDestinationBlock, set3, set4, set5, ifStmt));
        block_(
                labelledBlock_("SOME_BLOCK",
                        set_(),
                        set_()
                ),
                set_(),
                set_(),
                set_(),
                if_(block_(
                        block_(
                                jmpIf_(),
                                set_()
                        ),
                        set_()
                ), any_())
        ).verify(program);

        TreeSmith treeSmith = new TreeSmith(program);
        boolean eliminatedGoto = treeSmith.eliminateGoto(jumpTranspilerNode);
        assertTrue(eliminatedGoto);

        block_(
                loop_(
                        block_(
                                labelledBlock_("SOME_BLOCK",
                                        set_(),
                                        set_()
                                ),
                                set_(),
                                set_(),
                                set_(),
                                if_(block_(
                                        block_(
                                                set_(),
                                                if_(block_(
                                                        set_()
                                                ), any_())
                                        ),
                                        set_(),
                                        if_(block_(
                                                        set_()
                                                ), any_()
                                        )
                                ), any_())
                        )
                )
        ).verify(program);
    }

    @Test
    public void canIdentifyForwardJump() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode set5 = set("NOP", 80);
        TranspilerNode set6 = set("RST", 90);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("EFG"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        JumpIfTranspilerNode jumpTranspilerNode = new JumpIfTranspilerNode(new NamedLocationNode("SOME_BLOCK"), condition);
        TranspilerNode jumpDestinationBlock = new LabelledTranspilerCodeBlockNode("SOME_BLOCK", ImmutableList.of(set1, set2), ImmutableMap.of("type", FlowNodeType.PARAGRAPH));
        IfTranspilerNode ifStmt = new IfTranspilerNode(condition, new TranspilerCodeBlockNode(ImmutableList.of(new TranspilerCodeBlockNode(ImmutableList.of(jumpTranspilerNode, set6)), set("abcd", 12))));
        TranspilerNode program = new TranspilerCodeBlockNode(ImmutableList.of(ifStmt, set3, set4, set5, jumpDestinationBlock));
        block_(
                if_(block_(
                        block_(
                                jmpIf_(),
                                set_()
                        ),
                        set_()
                ), any_()),
                set_(),
                set_(),
                set_(),
                labelledBlock_("SOME_BLOCK",
                        set_(),
                        set_()
                )
        ).verify(program);

        TreeSmith treeSmith = new TreeSmith(program);
        assertEquals(JumpType.FORWARD, treeSmith.jumpType(jumpTranspilerNode));
    }

    @Test
    public void canIdentifyBackJump() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode set5 = set("NOP", 80);
        TranspilerNode set6 = set("RST", 90);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("EFG"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        JumpIfTranspilerNode jumpTranspilerNode = new JumpIfTranspilerNode(new NamedLocationNode("SOME_BLOCK"), condition);
        TranspilerNode jumpDestinationBlock = new LabelledTranspilerCodeBlockNode("SOME_BLOCK", ImmutableList.of(set1, set2), ImmutableMap.of("type", FlowNodeType.PARAGRAPH));
        IfTranspilerNode ifStmt = new IfTranspilerNode(condition, new TranspilerCodeBlockNode(ImmutableList.of(new TranspilerCodeBlockNode(ImmutableList.of(jumpTranspilerNode, set6)), set("abcd", 12))));
        TranspilerNode program = new TranspilerCodeBlockNode(ImmutableList.of(jumpDestinationBlock, set3, set4, set5, ifStmt));
        block_(
                labelledBlock_("SOME_BLOCK",
                        set_(),
                        set_()
                ),
                set_(),
                set_(),
                set_(),
                if_(block_(
                        block_(
                                jmpIf_(),
                                set_()
                        ),
                        set_()
                ), any_())
        ).verify(program);

        TreeSmith treeSmith = new TreeSmith(program);
        assertEquals(JumpType.BACKWARD, treeSmith.jumpType(jumpTranspilerNode));
    }

    private static SetTranspilerNode set(String variable, int value) {
        return new SetTranspilerNode(new SymbolReferenceNode(variable), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(value)));
    }
}
