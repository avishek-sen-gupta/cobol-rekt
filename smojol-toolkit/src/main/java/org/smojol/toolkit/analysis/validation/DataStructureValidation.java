package org.smojol.toolkit.analysis.validation;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.structure.UnreferencedVariableSearch;
import org.smojol.common.validation.ProgramValidationErrors;
import org.smojol.common.vm.strategy.UnresolvedReferenceDoNothingStrategy;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.DataStructureBuilder;
import org.smojol.common.vm.structure.Format1DataStructure;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;

import java.util.List;

public interface DataStructureValidation {
    ProgramValidationErrors validate(CobolEntityNavigator navigator, ParsePipeline pipeline, String programFilename);

    CobolDataStructure run(DataStructureBuilder dataStructureBuilder);


    DataStructureValidation BUILD = new DataStructureValidation() {
        @Override
        public ProgramValidationErrors validate(CobolEntityNavigator navigator, ParsePipeline pipeline, String programFilename) {
            List<ParseTree> usageSearchResults = new UnreferencedVariableSearch().run(navigator, pipeline.getDataStructures());
            if (usageSearchResults.isEmpty()) return ProgramValidationErrors.noError(programFilename);
            return ProgramValidationErrors.usageErrors(programFilename, usageSearchResults);
        }

        @Override
        public CobolDataStructure run(DataStructureBuilder dataStructureBuilder) {
            return dataStructureBuilder.build();
        }
    };

    DataStructureValidation NO_BUILD = new DataStructureValidation() {
        @Override
        public ProgramValidationErrors validate(CobolEntityNavigator navigator, ParsePipeline pipeline, String programFilename) {
            return ProgramValidationErrors.noError(programFilename);
        }

        @Override
        public CobolDataStructure run(DataStructureBuilder dataStructureBuilder) {
            return new Format1DataStructure(0, new UnresolvedReferenceDoNothingStrategy());
        }
    };
}
