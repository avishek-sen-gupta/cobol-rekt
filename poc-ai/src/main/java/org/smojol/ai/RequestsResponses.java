package org.smojol.ai;

import com.google.common.collect.ImmutableList;

import java.util.List;

public class RequestsResponses {
    public static final String PROMPT = "Assume that you are a mainframe COBOL expert. The following lines contain a piece of code from a legacy codebase in COBOL. Explain this code in as much detail as you can, while referring to the original statements and variables in your explanations. Break down your explanations paragraph by paragraph, sequentially, and refer to the paragraphs by their names. Explicit list the section, paragraph, or variable you are explaining. Every sentence must refer to at least one statement or sentence. Be verbose if you need to.\n";
    public static final List<String> SAMPLE_RESPONSE = ImmutableList.of(
            "The [U204-CALL-COST-PRICE] section in this COBOL code appears to handle various calculations and data assignments related to vendor pricing.",
            "In the [U204A] paragraph, the code checks if a vendor is the main vendor and performs various assignments based on that condition. If the vendor is the main vendor, it sets certain variables and then calls the [GET-TASI-2918] paragraph to retrieve data for that vendor. If the vendor is a vendor of TASI-2918, it sets a flag in [COPRI-IND-CHRYSLER].",
            "Following that, the code further assigns values to various fields based on conditions regarding the type of vendor. If the field [PRICE-INPUT] is true, it sets the [COPRI-SOURCE-IND] to 'M'. If a certain condition is met in the variable [VIDET-IND-ORGPRICE], it sets the [COPRI-SOURCE-IND] to different values.",
            "Afterwards, several variables are manipulated and updated through calculations and the execution of the [U1180] paragraph, followed by condition checks on [DISC-INPUT] and different variables to calculate values for [COPRI-VNP-DOC-VC].",
            "The code then computes values for [COPRI-VLP-DOC-VC], updates various fields related to cost, currency, date, and other details, and then sets several values for marketing and discount codes. It also initiates calls to external paragraphs like [UPDATE-COPRI] and [SRPACL01] based on conditions.",
            "Finally, in the [U204B] paragraph, certain values are updated and set based on the main vendor status and then control flows to either perform additional actions or exit the section.");
}
