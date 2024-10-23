package org.smojol.common;

import com.google.common.collect.ImmutableList;
import org.junit.jupiter.api.Test;
import org.smojol.common.list.CarCdr;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.expression.ConditionTestTime;
import org.smojol.common.vm.type.TypedRecord;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class TreeSmithTest {
    @Test
    public void canEscapeArbitraryScopeUsingJumpIf() {
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("abcd"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        TranspilerNode gotoSomeplace = new JumpTranspilerNode(new NamedLocationNode("SOMEPLACE"));
        IfTranspilerNode ifStmt = new IfTranspilerNode(condition, new TranspilerCodeBlockNode(ImmutableList.of(gotoSomeplace, set("abcd", 12))));
        TranspilerCodeBlockNode program = new TranspilerCodeBlockNode(ifStmt);
        TreeSmith treeOps = new TreeSmith(program);
        boolean escaped = treeOps.escapeScope(gotoSomeplace, ifStmt.getIfThenBlock());
        assertTrue(escaped);
    }

    @Test
    public void canFindRangeOfStatements() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode set5 = set("NOP", 80);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2, set3, set4, set5));

        assertEquals(ImmutableList.of(set2, set3, set4), parent.range(set2, set4));
    }

    @Test
    public void canEncloseStatementRangeInNewScope() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("EFG"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        JumpIfTranspilerNode jumpTranspilerNode = new JumpIfTranspilerNode(new NamedLocationNode("SOMEPLACE"), condition);
        TranspilerNode parent = new TranspilerCodeBlockNode(ImmutableList.of(set1, set2, set3, set4));

        List<TranspilerNode> range = parent.range(set2, set4);
        assertEquals(ImmutableList.of(set2, set3, set4), range);
        assertEquals(ImmutableList.of(set2, set3), CarCdr.init(range));
        TranspilerCodeBlockNode newScope = new TranspilerCodeBlockNode(CarCdr.init(range));
        TranspilerLoop loop = new TranspilerLoop(new SymbolReferenceNode("ABC"), new NullTranspilerNode(), new NullTranspilerNode(),
                jumpTranspilerNode.getCondition(), new NullTranspilerNode(), ConditionTestTime.AFTER, newScope);
    }

    private static SetTranspilerNode set(String variable, int value) {
        return new SetTranspilerNode(new SymbolReferenceNode(variable), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(value)));
    }
}
