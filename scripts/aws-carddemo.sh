myArray=("CBACT01C.cbl" "CBACT04C.cbl" "CBTRN03C.cbl" "CBACT03C.cbl" "CBSTM03B.CBL" "COADM01C.cbl" "COCRDSLC.cbl" "CORPT00C.cbl" "COTRN01C.cbl" "COUSR01C.cbl" "CSUTLDTC.cbl" "CBACT02C.cbl" "CBCUS01C.cbl" "CBTRN01C.cbl" "COACTUPC.cbl" "COBIL00C.cbl" "COCRDUPC.cbl" "COSGN00C.cbl" "COTRN02C.cbl" "COUSR02C.cbl" "CBACT03C.cbl" "CBSTM03A.CBL" "CBTRN02C.cbl" "COACTVWC.cbl" "COCRDLIC.cbl" "COMEN01C.cbl" "COTRN00C.cbl" "COUSR00C.cbl" "COUSR03C.cbl")
# "CBTRN03C.cbl"
# "CBACT03C.cbl"
. config-local/local.env.sh
for str in ${myArray[@]}; do
java -jar smojol-cli/target/smojol-cli.jar run $str --commands="DRAW_FLOWCHART" --srcDir /Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cbl --copyBooksDir "/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy,/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy-bms" --dialectJarPath che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar --dialect COBOL --reportDir out --generation=PARAGRAPH
done
