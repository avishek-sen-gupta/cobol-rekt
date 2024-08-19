package org.smojol.common.program;

import com.google.gson.annotations.Expose;
import hu.webarticum.treeprinter.SimpleTreeNode;
import hu.webarticum.treeprinter.TreeNode;
import lombok.Getter;
import org.smojol.common.ast.CallTarget;

import java.util.ArrayList;
import java.util.List;

public class CobolProgram extends SimpleTreeNode {
    @Expose @Getter private final CallTarget callTarget;
    @Expose @Getter private final List<CobolProgram> dependencies = new ArrayList<>();

    public CobolProgram(CallTarget callTarget) {
        super(callTarget.getName());
        this.callTarget = callTarget;
    }

    public void addAll(List<CobolProgram> dependencies) {
        this.dependencies.addAll(dependencies);
    }

    public List<CobolProgram> staticDependencies() {
        return dependencies.stream().filter(CobolProgram::isStatic).toList();
    }

    @Override
    public List<TreeNode> children() {
        return dependencies.stream().map(d -> (TreeNode) d).toList();
    }

    private boolean isStatic() {
        return callTarget.getClass() == StaticCallTarget.class;
    }

    public String getName() {
        return callTarget.getName();
    }

    @Override
    public String toString() {
        return callTarget.toString();
    }

    public void accept(ProgramVisitor visitor) {
        ProgramVisitor scopedVisitor = visitor.visit(this);
        dependencies.forEach(d -> d.accept(scopedVisitor));
    }
}
