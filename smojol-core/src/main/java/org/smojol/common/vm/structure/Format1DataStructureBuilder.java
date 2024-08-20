package org.smojol.common.vm.structure;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.structure.SourceSection;
import org.smojol.common.vm.strategy.UnresolvedReferenceStrategy;

public interface Format1DataStructureBuilder {
    Format1DataStructure build(CobolParser.DataDescriptionEntryFormat1Context format1Structure, UnresolvedReferenceStrategy strategy, SourceSection sourceSection);
}
