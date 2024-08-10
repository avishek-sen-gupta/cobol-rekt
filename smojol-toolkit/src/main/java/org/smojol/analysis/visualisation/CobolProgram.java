package org.smojol.analysis.visualisation;

import lombok.Getter;
import org.smojol.ast.CallTarget;
import org.smojol.ast.StaticCallTarget;

import java.util.ArrayList;
import java.util.List;

public class CobolProgram {
    @Getter
    private CallTarget callTarget;
    @Getter
    private List<CobolProgram> dependencies = new ArrayList<>();

    public CobolProgram(CallTarget callTarget) {
        this.callTarget = callTarget;
    }

    public void addAll(List<CobolProgram> dependencies) {
        this.dependencies.addAll(dependencies);
    }

    public List<CobolProgram> staticDependencies() {
        return dependencies.stream().filter(CobolProgram::isStatic).toList();
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
}
