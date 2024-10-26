package org.smojol.common;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import org.junit.jupiter.api.Test;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.list.CarCdr;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class TreeSmithTest {
    @Test
    public void canEscapeArbitraryScopeUsingJumpIfForForwardJump() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("abcd"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        TranspilerNode gotoSomeplace = new JumpTranspilerNode(new NamedLocationNode("SOMEPLACE"));
        IfTranspilerNode ifStmt = new IfTranspilerNode(condition, new TranspilerCodeBlockNode(ImmutableList.of(gotoSomeplace, set("abcd", 12))));
        TranspilerCodeBlockNode program = new TranspilerCodeBlockNode(ImmutableList.of(ifStmt, set1, set2));
        TreeMatcher blockMatcher = block_(
                if_(block_(
                                jmp_(),
                                set_()),
                        any_()),
                set_(),
                set_()
                );
        blockMatcher.run(program).assertStructure();
        TreeSmith treeOps = new TreeSmith(program);
        boolean escaped = treeOps.escapeScope(gotoSomeplace);
        List<TranspilerNode> children = program.astChildren();
        TreeMatcher postTransformBlockMatcher = block_(
                if_(block_(
                                set_(),
                                if_(block_(
                                        jmp_(),
                                        set_()
                                ), any_())
                        ),
                        any_()),
                jmpIf_(),
                set_(),
                set_()
        );
        postTransformBlockMatcher.run(program).assertStructure();
        assertTrue(children.getFirst() instanceof IfTranspilerNode);
        assertTrue(children.get(1) instanceof JumpIfTranspilerNode);
        assertTrue(escaped);
    }

    @Test
    public void canAssertStructure() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TreeMatcher blockMatcher = block_(
                set_(),
                set_(),
                if_(block_(
                        jmp_(),
                        set_()),
                    any_()));
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("abcd"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        TranspilerNode gotoSomeplace = new JumpTranspilerNode(new NamedLocationNode("SOMEPLACE"));
        IfTranspilerNode ifStmt = new IfTranspilerNode(condition, new TranspilerCodeBlockNode(ImmutableList.of(gotoSomeplace, set("abcd", 12))));
        TranspilerCodeBlockNode blockNode = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2, ifStmt));
        TreeMatchResult matchResult = blockMatcher.run(blockNode);
        matchResult.assertStructure();

    }

    private TreeMatcher jmpIf_() {
        return new TreeMatcher(n -> n instanceof JumpIfTranspilerNode);
    }

    private TreeMatcher jmp_() {
        return new TreeMatcher(n -> n instanceof JumpTranspilerNode);
    }

    private TreeMatcher any_() {
        return new TreeMatcher(n -> n != null);
    }

    private TreeMatcher if_(TreeMatcher ifThen, TreeMatcher ifElse) {
        return new TreeMatcher(n -> n instanceof IfTranspilerNode, ImmutableList.of(ifThen, ifElse));
    }

    private static TreeMatcher set_() {
        return new TreeMatcher(n -> n instanceof SetTranspilerNode);
    }

//    private static TreeMatcher blockNode(List<TreeMatcher> childMatchers) {
//        return new TreeMatcher(n -> n instanceof TranspilerCodeBlockNode, childMatchers);
//    }

    private static TreeMatcher block_(TreeMatcher... childMatchers) {
        return new TreeMatcher(n -> n instanceof TranspilerCodeBlockNode, Arrays.asList(childMatchers));
    }

    private List<TreeMatcher> list() {
        return null;
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
    public void canEncloseStatementRangeInNewScopeAutomaticallyGivenJumpIfNodeForBackJump() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("EFG"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        JumpIfTranspilerNode jumpTranspilerNode = new JumpIfTranspilerNode(new NamedLocationNode("SOME_BLOCK"), condition);
        TranspilerNode jumpBlock = new LabelledTranspilerCodeBlockNode("SOME_BLOCK", ImmutableList.of(set1, set2), ImmutableMap.of("type", FlowNodeType.PARAGRAPH));
        TranspilerNode program = new TranspilerCodeBlockNode(ImmutableList.of(jumpBlock, set3, set4, jumpTranspilerNode));
        assertTrue(new TreeSmith(program).eliminateBackJump(jumpTranspilerNode));
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
        assertTrue(new TreeSmith(program).eliminateForwardJump(jumpTranspilerNode));
    }

    private static SetTranspilerNode set(String variable, int value) {
        return new SetTranspilerNode(new SymbolReferenceNode(variable), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(value)));
    }
}
