set -e
myArray=("CBACT01C.cbl" "CBSTM03A.CBL " "CBACT04C.cbl" "CBTRN03C.cbl" "CBACT03C.cbl" "CBSTM03B.CBL" "COADM01C.cbl" "COCRDSLC.cbl" "CORPT00C.cbl" "COTRN01C.cbl" "COUSR01C.cbl"  "CBACT02C.cbl" "CSUTLDTC.cbl" "CBCUS01C.cbl" "CBTRN01C.cbl" "COACTUPC.cbl" "COBIL00C.cbl" "COCRDUPC.cbl" "COSGN00C.cbl" "COTRN02C.cbl" "COUSR02C.cbl" "CBACT03C.cbl" "CBTRN02C.cbl" "COACTVWC.cbl" "COCRDLIC.cbl" "COMEN01C.cbl" "COTRN00C.cbl" "COUSR00C.cbl" "COUSR03C.cbl")

. config-local/local.env.sh


# "CSUTLDTC.cbl"
# "CBSTM03A.CBL" POINTER data type needs to be accomodated while building data structures
for str in ${myArray[@]}; do
    source config-local/local.env.sh && java -Djava.util.logging.config.file="/Users/asgupta/code/smojol/logging.properties" -jar smojol-cli/target/smojol-cli.jar run $str --commands="BUILD_TRANSPILER_MODEL EXPORT_TO_GRAPHML WRITE_FLOW_AST WRITE_CFG WRITE_DATA_STRUCTURES EXPORT_UNIFIED_TO_JSON INJECT_INTO_NEO4J BUILD_PROGRAM_DEPENDENCIES" --srcDir /Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cbl --copyBooksDir "/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy,/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy-bms" --dialectJarPath che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar --dialect COBOL --reportDir out --generation=PARAGRAPH

    # source config-local/local.env.sh && java -jar smojol-cli/target/smojol-cli.jar run $str --commands="INJECT_INTO_NEO4J" --srcDir /Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cbl --copyBooksDir "/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy,/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy-bms" --dialectJarPath che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar --dialect COBOL --reportDir out --generation=PARAGRAPH

    # source config-local/local.env.sh && java -jar smojol-cli/target/smojol-cli.jar dependency $str --srcDir /Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cbl --copyBooksDir "/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy,/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy-bms" --dialectJarPath che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar --dialect COBOL --neo4j --permissiveSearch
done
