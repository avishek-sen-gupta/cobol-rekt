package org.smojol.common;

import com.google.common.collect.ImmutableList;
import org.junit.jupiter.api.Test;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.smojol.common.TreeMatcher.*;

public class ZipperTest {
    @Test
    public void canModifyTranspilerNodesUsingZipper() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode set5 = set("NOP", 80);
        TranspilerNode set6 = set("RST", 90);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("EFG"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        JumpIfTranspilerNode jumpTranspilerNode = new JumpIfTranspilerNode(new NamedLocationNode("SOME_BLOCK"), condition);
        IfTranspilerNode ifStmt = new IfTranspilerNode(condition, new TranspilerCodeBlockNode(ImmutableList.of(new TranspilerCodeBlockNode(ImmutableList.of(jumpTranspilerNode, set6)), set("abcd", 12))));
        TranspilerCodeBlockNode block = new TranspilerCodeBlockNode(ImmutableList.of(set4, set5));
        TranspilerNode program = new TranspilerCodeBlockNode(ImmutableList.of(set1, set3, block, ifStmt));

        block_(
                set_(),
                set_(),
                block_(
                        set_(),
                        set_()
                ),
                if_(block_(
                        block_(
                                jmpIf_(),
                                set_()
                        ),
                        set_()
                ), any_())
        ).verify(program);

        Zipper<TranspilerNode> zippy = new Zipper<>(io.vavr.collection.List.of(), program, TranspilerCloneOperation::clone);
        Zipper<TranspilerNode> down = zippy.down(set1);

        assertEquals(io.vavr.collection.List.of(program), down.getThread());
        assertEquals(set1, down.getCurrent());


        Zipper<TranspilerNode> insideBlock = zippy.down(block).down(set4);
        assertEquals(io.vavr.collection.List.of(block, program), insideBlock.getThread());
        assertEquals(set4, insideBlock.getCurrent());

        Zipper<TranspilerNode> backUpToBlock = insideBlock.up();
        assertEquals(io.vavr.collection.List.of(program), backUpToBlock.getThread());
        assertEquals(block, backUpToBlock.getCurrent());
        Zipper<TranspilerNode> newTranspilerZipper = backUpToBlock.replaceChildren(io.vavr.collection.List.of(set2));

        block_(
                set_(),
                set_(),
                block_(
                        set_(),
                        set_()
                ),
                if_(block_(
                        block_(
                                jmpIf_(),
                                set_()
                        ),
                        set_()
                ), any_())
        ).verify(program);

        block_(
                set_(),
                set_(),
                block_(
                        set_()
                ),
                if_(block_(
                        block_(
                                jmpIf_(),
                                set_()
                        ),
                        set_()
                ), any_())
        ).verify(newTranspilerZipper.getCurrent());
    }

    @Test
    public void canTraverseTranspilerNodesUsingZipper() {
        TranspilerNode set1 = set("ABC", 30);
        TranspilerNode set2 = set("DEF", 40);
        TranspilerNode set3 = set("PQR", 50);
        TranspilerNode set4 = set("KLM", 70);
        TranspilerNode set5 = set("NOP", 80);
        TranspilerNode set6 = set("RST", 90);
        EqualToNode condition = new EqualToNode(new SymbolReferenceNode("EFG"), new PrimitiveValueTranspilerNode(TypedRecord.TRUE));
        JumpIfTranspilerNode jumpTranspilerNode = new JumpIfTranspilerNode(new NamedLocationNode("SOME_BLOCK"), condition);
        IfTranspilerNode ifStmt = new IfTranspilerNode(condition, new TranspilerCodeBlockNode(ImmutableList.of(new TranspilerCodeBlockNode(ImmutableList.of(jumpTranspilerNode, set6)), set("abcd", 12))));
        TranspilerCodeBlockNode block = new TranspilerCodeBlockNode(ImmutableList.of(set4, set5));
        TranspilerNode program = new TranspilerCodeBlockNode(ImmutableList.of(set1, set3, block, ifStmt));

        Zipper<TranspilerNode> zippy = new Zipper<>(io.vavr.collection.List.of(), program, TranspilerCloneOperation::clone);
        Zipper<TranspilerNode> down = zippy.down(set1);

        assertEquals(io.vavr.collection.List.of(program), down.getThread());
        assertEquals(set1, down.getCurrent());


        Zipper<TranspilerNode> insideBlock = zippy.down(block).down(set4);
        assertEquals(io.vavr.collection.List.of(block, program), insideBlock.getThread());
        assertEquals(set4, insideBlock.getCurrent());

        Zipper<TranspilerNode> backUpToBlock = insideBlock.up();
        assertEquals(io.vavr.collection.List.of(program), backUpToBlock.getThread());
        assertEquals(block, backUpToBlock.getCurrent());

        block_(
                set_(),
                set_(),
                block_(
                        set_(),
                        set_()
                ),
                if_(block_(
                        block_(
                                jmpIf_(),
                                set_()
                        ),
                        set_()
                ), any_())
        ).verify(program);
    }

    @Test
    public void canUseZipperNatively() {
        NativeZipperNode nn = n("A",
                n("B"),
                n("C",
                        n("D"),
                        n("E")
                )
        );

        NativeZipper<NativeZipperNode> zippy = new NativeZipper<>(io.vavr.collection.List.of(), nn);
        NativeZipper<NativeZipperNode> atB = zippy.down(n -> n.id().equals("B"));
        NativeZipper<NativeZipperNode> atRoot = atB.replaceChildren(io.vavr.collection.List.of(n("B1"), n("B2")));
        NativeZipperNode newRootZipper = atRoot.current();
        NativeZipperNode nodeB = newRootZipper.astChildren().get(0);
        assertNotSame(atB.getCurrent().astChildren().size(), nodeB.astChildren().size());
        assertEquals("B", nodeB.id());
        assertEquals("B1", nodeB.astChildren().get(0).id());
        assertEquals("B2", nodeB.astChildren().get(1).id());
    }

    private NativeZipperNode n(String id, NativeZipperNode... children) {
        return new NativeZipperNode(id, io.vavr.collection.List.of(children));
    }

    private static SetTranspilerNode set(String variable, int value) {
        return new SetTranspilerNode(new SymbolReferenceNode(variable), new PrimitiveValueTranspilerNode(TypedRecord.typedNumber(value)));
    }
}
