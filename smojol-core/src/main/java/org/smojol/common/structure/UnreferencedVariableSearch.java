package org.smojol.common.structure;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.NullDataStructure;

import java.util.List;

public class UnreferencedVariableSearch {
    public List<ParseTree> run(CobolEntityNavigator navigator, CobolDataStructure dataStructures) {
        List<ParseTree> allVariableUsages = navigator.findAllByCondition(n -> n.getClass() == CobolParser.VariableUsageNameContext.class);
        return allVariableUsages.stream().filter(gid -> dataStructures.reference(gid.getText()).getClass() == NullDataStructure.class).distinct().toList();
    }
}
