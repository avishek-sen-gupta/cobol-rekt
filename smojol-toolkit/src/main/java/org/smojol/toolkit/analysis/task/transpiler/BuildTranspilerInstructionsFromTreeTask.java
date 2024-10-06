package org.smojol.toolkit.analysis.task.transpiler;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.id.IncrementingIdProvider;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.transpiler.*;
import org.smojol.common.transpiler.instruction.ConditionalJumpInstruction;
import org.smojol.common.transpiler.instruction.DetachedInstructionBlockMarker;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.intermediate.IntermediateASTNodeBuilder;
import org.smojol.toolkit.transpiler.TranspilerTreeBuilder;

import java.util.ArrayList;
import java.util.List;

import static org.smojol.common.pseudocode.CodeSentinelType.*;

public class BuildTranspilerInstructionsFromTreeTask {
    private final ParseTree rawAST;
    private final CobolDataStructure dataStructures;
    private final SmojolSymbolTable symbolTable;
    private final IncrementingIdProvider idProvider;

    public BuildTranspilerInstructionsFromTreeTask(ParseTree rawAST, CobolDataStructure dataStructures, SmojolSymbolTable symbolTable) {
        this.rawAST = rawAST;
        this.dataStructures = dataStructures;
        this.symbolTable = symbolTable;
        idProvider = new IncrementingIdProvider();
    }

    public List<TranspilerInstruction> run() {
        FlowNode flowRoot = new IntermediateASTNodeBuilder(rawAST, dataStructures, symbolTable).build();
        TranspilerNode transpilerTree = TranspilerTreeBuilder.flowToTranspiler(flowRoot, dataStructures);
//        TranspilerInstructionGeneratorVisitor visitor = new TranspilerInstructionGeneratorVisitor(idProvider);
        return buildInstructions(transpilerTree);

//        new AggregatingTranspilerNodeTraversal<List<TranspilerInstruction>>().accept(transpilerTree, visitor);
//        return visitor.result();

    }

    private List<TranspilerInstruction> buildInstructions(TranspilerNode node) {
        return buildInstructions(node, ImmutableList.of());
    }

    private List<TranspilerInstruction> buildInstructions(TranspilerNode node, List<TranspilerInstruction> epilogue) {
        return switch (node) {
            case LabelledTranspilerCodeBlockNode cb -> composite(cb, epilogue);
            case DetachedTranspilerCodeBlockNode cb -> composite(cb, epilogue);
            case TranspilerCodeBlockNode cb -> composite(cb, epilogue);
            case IfTranspilerNode ifStmt -> ifInstructions(ifStmt, epilogue);
            case TranspilerLoop loop -> loopInstructions(loop, epilogue);
            default -> atomicUnit(node);
        };
    }

    private List<TranspilerInstruction> loopInstructions(TranspilerLoop loop, List<TranspilerInstruction> epilogue) {
        return null;
    }

    private List<TranspilerInstruction> atomicUnit(TranspilerNode node) {
        String instructionID = idProvider.next();
        return ImmutableList.of(
                new TranspilerInstruction(node, ENTER, instructionID),
                new TranspilerInstruction(node, BODY, instructionID),
                new TranspilerInstruction(node, EXIT, instructionID));
    }

    private List<TranspilerInstruction> ifInstructions(IfTranspilerNode ifStmt, List<TranspilerInstruction> epilogue) {
        String jumpInstructionID = idProvider.next();
        List<TranspilerInstruction> allInstructions = new ArrayList<>();

        IdLocationNode ifExitLocation = new IdLocationNode(ifStmt, EXIT);
        List<TranspilerInstruction> ifThenScope = buildInstructions(ifStmt.getIfThenBlock(), atomicUnit(new JumpTranspilerNode(ifExitLocation)));
        List<TranspilerInstruction> ifElseScope = buildInstructions(ifStmt.getIfElseBlock(), atomicUnit(new JumpTranspilerNode(ifExitLocation)));
        ConditionalJumpInstruction cjEntry = new ConditionalJumpInstruction(ifStmt,
                new IdLocationNode(ifThenScope.getFirst().ref(), ENTER),
                new IdLocationNode(ifElseScope.getFirst().ref(), ENTER),
                ENTER, jumpInstructionID);
        ConditionalJumpInstruction cjExit = new ConditionalJumpInstruction(ifStmt,
                new IdLocationNode(ifThenScope.getFirst().ref(), ENTER),
                new IdLocationNode(ifElseScope.getFirst().ref(), ENTER),
                EXIT, jumpInstructionID);

        allInstructions.add(cjEntry);
        allInstructions.addAll(ifThenScope);
        allInstructions.addAll(ifElseScope);
        allInstructions.add(cjExit);

        return allInstructions;
    }

    private List<TranspilerInstruction> composite(TranspilerNode cb, List<TranspilerInstruction> epilogue) {
        List<TranspilerInstruction> allInstructions = new ArrayList<>();
        String scopeID = idProvider.next();
        DetachedInstructionBlockMarker scopeEnter = new DetachedInstructionBlockMarker(cb, ENTER, scopeID);
        DetachedInstructionBlockMarker scopeBody = new DetachedInstructionBlockMarker(cb, BODY, scopeID);
        List<TranspilerInstruction> childInstructions = cb.astChildren().stream().flatMap(c -> buildInstructions(c).stream()).toList();
        DetachedInstructionBlockMarker scopeExit = new DetachedInstructionBlockMarker(cb, EXIT, scopeID);

        allInstructions.add(scopeEnter);
        allInstructions.add(scopeBody);
        allInstructions.addAll(childInstructions);
        allInstructions.addAll(epilogue);
        allInstructions.add(scopeExit);

        return allInstructions;
    }
}
