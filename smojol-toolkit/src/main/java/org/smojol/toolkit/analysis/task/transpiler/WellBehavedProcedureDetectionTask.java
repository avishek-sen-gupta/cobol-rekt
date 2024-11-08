package org.smojol.toolkit.analysis.task.transpiler;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.transpiler.*;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class WellBehavedProcedureDetectionTask {
    private final TranspilerCodeBlockNode root;
    public WellBehavedProcedureDetectionTask(TranspilerCodeBlockNode root) {
        this.root = root;
    }

    public Set<TranspilerNode> run() {
        List<TranspilerNode> sections = root.findAllRecursive(n -> n instanceof LabelledTranspilerCodeBlockNode l && FlowNodeType.SECTION == l.getProperty("type"));
        Set<Pair<TranspilerNode, Set<TranspilerNode>>> procedureMap = sections.stream()
                .map(section -> (Pair<TranspilerNode, Set<TranspilerNode>>) ImmutablePair.of(section,
                        section.findAllRecursive(n -> n instanceof LabelledTranspilerCodeBlockNode l
                                && FlowNodeType.PARAGRAPH == l.getProperty("type")).stream()
                                .collect(Collectors.toUnmodifiableSet())))
                .collect(Collectors.toUnmodifiableSet());
        Set<Pair<TranspilerNode, Set<TranspilerNode>>> wellBehavedLeafPairs = procedureMap.stream().filter(procMap -> isWellBehavedLeafProcedure(procMap, procedureMap)).collect(Collectors.toUnmodifiableSet());
        Set<Pair<TranspilerNode, Set<TranspilerNode>>> unclassifiedProcedurePairs = Sets.difference(procedureMap, wellBehavedLeafPairs);
        Set<Pair<TranspilerNode, Set<TranspilerNode>>> wellBehavedProcedurePairs = xxxx(unclassifiedProcedurePairs, procedureMap, wellBehavedLeafPairs);
        return wellBehavedProcedurePairs.stream().map(Pair::getLeft).collect(Collectors.toUnmodifiableSet());
    }

    private Set<Pair<TranspilerNode, Set<TranspilerNode>>> xxxx(Set<Pair<TranspilerNode, Set<TranspilerNode>>> unclassifiedProcedurePairs, Set<Pair<TranspilerNode, Set<TranspilerNode>>> fullProcedureMap, Set<Pair<TranspilerNode, Set<TranspilerNode>>> currentWellBehavedProcPairs) {
        if (currentWellBehavedProcPairs.isEmpty()) return ImmutableSet.of();
        Set<Pair<TranspilerNode, Set<TranspilerNode>>> newWellBehavedProcedures = unclassifiedProcedurePairs.stream().filter(procMap -> isWellBehavedProcedure(procMap, fullProcedureMap, currentWellBehavedProcPairs)).collect(Collectors.toUnmodifiableSet());
        return Sets.union(currentWellBehavedProcPairs, xxxx(Sets.difference(unclassifiedProcedurePairs, newWellBehavedProcedures), fullProcedureMap, newWellBehavedProcedures));
    }

    private boolean isWellBehavedProcedure(Pair<TranspilerNode, Set<TranspilerNode>> procMap, Set<Pair<TranspilerNode, Set<TranspilerNode>>> proceduresMap, Set<Pair<TranspilerNode, Set<TranspilerNode>>> wellBehavedProcedures) {
        TranspilerNode procNode = procMap.getLeft();
        Set<TranspilerNode> allGotos = procNode.findAllRecursive(n -> n instanceof JumpTranspilerNode j && j.getEnd() == LocationNode.NULL).stream().collect(Collectors.toUnmodifiableSet());
        Set<TranspilerNode> allCalls = procNode.findAllRecursive(n -> n instanceof JumpTranspilerNode j && j.getEnd() != LocationNode.NULL).stream().collect(Collectors.toUnmodifiableSet());
        Set<LocationNode> allGotoDestinations = allGotos.stream().map(goto_ -> ((JumpTranspilerNode) goto_).getStart()).collect(Collectors.toUnmodifiableSet());
        Set<Pair<TranspilerNode, Set<TranspilerNode>>> otherSections = proceduresMap.stream().filter(pMap -> pMap.getLeft() != procNode).collect(Collectors.toUnmodifiableSet());
        Set<TranspilerNode> otherParagraphs = otherSections.stream().flatMap(os -> os.getRight().stream()).collect(Collectors.toUnmodifiableSet());
        Set<TranspilerNode> otherDestinationNodes = Stream.concat(otherSections.stream().map(Pair::getLeft), otherParagraphs.stream()).collect(Collectors.toUnmodifiableSet());
        Set<String> allGotoDestinationNames = allGotoDestinations.stream().map(gd -> ((NamedLocationNode) gd).getName()).collect(Collectors.toUnmodifiableSet());
        Set<String> allOtherLabelledBlockNames = otherDestinationNodes.stream().map(odn -> ((LabelledTranspilerCodeBlockNode) odn).getName()).collect(Collectors.toUnmodifiableSet());
        Set<String> callTargetNames = allCalls.stream().flatMap(call -> Stream.of(((JumpTranspilerNode) call).getStart(), ((JumpTranspilerNode) call).getEnd()))
                .map(ln -> ((NamedLocationNode) ln).getName()).collect(Collectors.toUnmodifiableSet());
        Set<String> wellBehavedProcedureNames = wellBehavedProcedures.stream().map(wbp -> ((LabelledTranspilerCodeBlockNode) wbp.getLeft()).getName()).collect(Collectors.toUnmodifiableSet());

        return Sets.intersection(allGotoDestinationNames, allOtherLabelledBlockNames).isEmpty()
                && Sets.difference(callTargetNames, wellBehavedProcedureNames).isEmpty();
    }

    private boolean isWellBehavedLeafProcedure(Pair<TranspilerNode, Set<TranspilerNode>> procMap, Set<Pair<TranspilerNode, Set<TranspilerNode>>> proceduresMap) {
        TranspilerNode procNode = procMap.getLeft();
        Set<TranspilerNode> allJumps = procNode.findAllRecursive(n -> n instanceof JumpTranspilerNode).stream().collect(Collectors.toUnmodifiableSet());
        return noJumpsToOtherSections(proceduresMap, allJumps, procNode);
    }

    private static boolean noJumpsToOtherSections(Set<Pair<TranspilerNode, Set<TranspilerNode>>> proceduresMap, Set<TranspilerNode> allJumps, TranspilerNode procNode) {
        Set<LocationNode> allGotoDestinations = allJumps.stream().map(goto_ -> ((JumpTranspilerNode) goto_).getStart()).collect(Collectors.toUnmodifiableSet());
        Set<Pair<TranspilerNode, Set<TranspilerNode>>> otherSections = proceduresMap.stream().filter(pMap -> pMap.getLeft() != procNode).collect(Collectors.toUnmodifiableSet());
        Set<TranspilerNode> otherParagraphs = otherSections.stream().flatMap(os -> os.getRight().stream()).collect(Collectors.toUnmodifiableSet());
        Set<TranspilerNode> otherDestinationNodes = Stream.concat(otherSections.stream().map(Pair::getLeft), otherParagraphs.stream()).collect(Collectors.toUnmodifiableSet());
        Set<String> allGotoDestinationNames = allGotoDestinations.stream().map(gd -> ((NamedLocationNode) gd).getName()).collect(Collectors.toUnmodifiableSet());
        Set<String> allLabelledBlockNames = otherDestinationNodes.stream().map(odn -> ((LabelledTranspilerCodeBlockNode) odn).getName()).collect(Collectors.toUnmodifiableSet());
        return Sets.intersection(allGotoDestinationNames, allLabelledBlockNames).isEmpty();
    }
}
