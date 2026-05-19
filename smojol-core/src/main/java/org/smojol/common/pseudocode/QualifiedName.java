// smojol-core/src/main/java/org/smojol/common/pseudocode/QualifiedName.java
package org.smojol.common.pseudocode;

import java.util.List;
import java.util.stream.Stream;

public record QualifiedName(List<String> parts) {

    // parts = [bareName, qualifier1, qualifier2, ...] innermost-first
    // e.g. "FIELD-A OF STRUCT-1" → parts = ["FIELD-A", "STRUCT-1"]

    public static QualifiedName of(String bareName) {
        return new QualifiedName(List.of(bareName));
    }

    // qualifiers: innermost-first as they appear after the first OF/IN in source
    // e.g. "FIELD-A OF STRUCT-1 OF ROOT" → bareName="FIELD-A", qualifiers=["STRUCT-1", "ROOT"]
    public static QualifiedName of(String bareName, List<String> qualifiers) {
        return new QualifiedName(
            Stream.concat(Stream.of(bareName), qualifiers.stream()).toList()
        );
    }

    public String bareName() {
        return parts.get(0);
    }

    /**
     * Returns true if this QualifiedName is a suffix-subsequence of {@code candidate}.
     *
     * {@code candidate} = full root-to-leaf path, outermost-first, e.g. ["ROOT", "STRUCT-1", "FIELD-A"].
     * {@code parts}     = [bareName, qualifier1, ...] innermost-first.
     *
     * The check: reverse parts to outermost-first order, then verify they form a
     * (non-contiguous) subsequence that ends at the tail of candidate.
     * The last element of the reversed query must match the last element of candidate.
     */
    public boolean isSuffixMatchedBy(List<String> candidate) {
        var query = parts.reversed(); // now outermost-first: [..., qualifier1, bareName]
        return isSuffixSubsequence(query, candidate);
    }

    private static boolean isSuffixSubsequence(List<String> query, List<String> candidate) {
        if (query.isEmpty()) return true;
        if (candidate.isEmpty()) return false;
        var qHead = query.get(0);
        var qTail = query.subList(1, query.size());
        for (int i = 0; i < candidate.size(); i++) {
            if (candidate.get(i).equalsIgnoreCase(qHead)) {
                if (qTail.isEmpty()) {
                    // bareName must land at the last position
                    if (i == candidate.size() - 1) return true;
                    // otherwise keep scanning for a later occurrence
                } else {
                    if (isSuffixSubsequence(qTail, candidate.subList(i + 1, candidate.size()))) {
                        return true;
                    }
                    // recursive call failed — keep scanning for another occurrence of qHead
                }
            }
        }
        return false;
    }
}
