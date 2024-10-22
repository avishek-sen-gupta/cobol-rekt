package org.smojol.common;

import com.google.common.collect.ImmutableList;
import org.junit.jupiter.api.Test;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;

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

    private static SetTranspilerNode set(String variable, int value) {
        return new SetTranspilerNode(new SymbolReferenceNode(variable), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(value)));
    }
}
