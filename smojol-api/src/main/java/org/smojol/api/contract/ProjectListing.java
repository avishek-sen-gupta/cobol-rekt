package org.smojol.api.contract;

import java.util.List;

public record ProjectListing(String projectID,
                             List<IntermediateASTListing> astListings,
                             List<IntermediateCFGListing> cfgListings,
                             List<UnifiedFlowModelListing> unifiedFlowModelListings) {
}
