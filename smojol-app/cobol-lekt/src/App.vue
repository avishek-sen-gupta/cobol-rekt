<script>
import HelloWorld from "@/components/HelloWorld.vue";
import axios from "axios";
import ProjectsView from "@/components/ProjectsView.vue";
import GraphView from "@/components/GraphView.vue";
import InfoPane from "@/components/InfoPane.vue";
import CodePane from "@/components/CodePane.vue";
import {flip} from "@/ts/FlippableId";
import {unifiedModelToDigraph} from "@/ts/UnifiedFlowModel";
import {instance} from "@viz-js/viz";

export default {
  name: 'App',
  components: {
    CodePane,
    InfoPane,
    GraphView,
    HelloWorld,
    ProjectsView
  },
  setup() {
  },
  data() {
    return {
      heartbeatResult: "UNKNOWN",
      irAST: null,
      irCFG: null,
      nodeDetails: null,
      centerNode: null,
      loopBodies: [],
      t1t2Result: null,
      flowModel: null
    };
  },
  mounted() {
    instance().then(viz => {
      const svg = viz.renderSVGElement(`
      digraph "example1" {
"a01fdc95-fbb4-4b98-b08d-5fb144465d7a" ["style"="filled","fontcolor"="white","fillcolor"="deepskyblue4","shape"="box","label"="ProcedureDivisionBodyContext/a01fdc95-fbb4-4b98-b08d-5fb144465d7a"]
"074de610-9dd6-45d6-8c96-a38d4ef4c0bb" ["label"="para-group:","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="point"]
"9f2d4205-e870-4dd2-9f27-f772f2eb472e" ["label"="SECTION-0","style"="filled","fontcolor"="white","fillcolor"="deepskyblue4","shape"="box"]
"9815f35f-6506-4f93-9557-eb527d806326" ["label"="SECTION-A","style"="filled","fontcolor"="white","fillcolor"="deepskyblue4","shape"="box"]
"76fdd3b8-0490-44dc-b8f4-92d5a1cde642" ["label"="SECTION-B","style"="filled","fontcolor"="white","fillcolor"="deepskyblue4","shape"="box"]
"4113107a-95cc-4454-b245-60390fac5875" ["label"="SECTION-B1","style"="filled","fontcolor"="white","fillcolor"="deepskyblue4","shape"="box"]
"d9d5a0d1-04b0-417c-9a37-2de905217aa6" ["label"="SECTION-C","style"="filled","fontcolor"="white","fillcolor"="deepskyblue4","shape"="box"]
"f79d7df3-fa45-42fc-9790-eeb538621f34" ["label"="para-group:","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="point"]
"b74c5262-3736-4c77-83b9-e427e0c5afc1" ["label"="P9","style"="filled","fontcolor"="white","fillcolor"="darkseagreen4","shape"="box"]
"db2f7768-f1a3-4e5d-a703-b8cac31a41e1" ["label"="P10","style"="filled","fontcolor"="white","fillcolor"="darkseagreen4","shape"="box"]
"94388d97-e553-45d5-a8a0-1ba64a1232c5" ["label"="IF(SOMEFRACTION","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="point"]
"7413e668-3333-4c94-b3df-992267c29e81" ["label"="IFSOMEFRACTION(","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="point"]
"cf4d94bc-3e47-4807-ad99-fe6eac5a5966" ["label"="STOPRUN.","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="point"]
"c83efbe3-3d2e-4ecf-8430-34092b047050" ["label"="STOPRUN","style"="filled","fontcolor"="white","fillcolor"="deepskyblue","shape"="circle"]
"4e859b75-f468-4733-9147-565a902729bb" ["label"="IS \\nSOMEFRACTION(1) = 100 OR 30 OR 50?\\n","style"="filled","fontcolor"="white","fillcolor"="chocolate4","shape"="diamond"]
"8eac8925-5678-4794-a9bf-14f4df732a8a" ["label"="Yes","style"="filled","fontcolor"="white","fillcolor"="darkgreen","shape"="doublecircle"]
"cae8f4b2-91f8-43d8-9ae1-c3b20e9e3038" ["label"="DISPLAY\\"AMAZE3\\"","style"="filled","fontcolor"="white","fillcolor"="chocolate4","shape"="point"]
"a0bd9989-7a16-4575-aa14-35aaad8a61ae" ["label"="IS \\n(SOMEFRACTION(1) = 10) OR >20 AND V1?\\n","style"="filled","fontcolor"="white","fillcolor"="chocolate4","shape"="diamond"]
"bfc7bc99-274f-4a40-a6e6-28a0664096ab" ["label"="Yes","style"="filled","fontcolor"="white","fillcolor"="darkgreen","shape"="doublecircle"]
"aec2b484-6fa0-480a-b73e-60fff928a17b" ["label"="DISPLAY\\"AMAZE\\"","style"="filled","fontcolor"="white","fillcolor"="chocolate4","shape"="point"]
"Processing Block: 81982782-9662-484a-82c0-07d391e0c491Processing\\n------------------------\\nDISPLAY \\"SOME-UNION-1 = \\" SOME-UNION-1.\\nMOVE 50 TO SOMEFRACTION(1).\\nMOVE \\"ABCD\\" TO SOMETHING.\\nMOVE \\"E\\" TO CONDI.\\n/81982782-9662-484a-82c0-07d391e0c491" ["label"="Processing\\n------------------------\\nDISPLAY \\"SOME-UNION-1 = \\" SOME-UNION-1.\\nMOVE 50 TO SOMEFRACTION(1).\\nMOVE \\"ABCD\\" TO SOMETHING.\\nMOVE \\"E\\" TO CONDI.\\n","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="box"]
"d037bbf4-eabb-477c-84ec-bf7dc37e6579" ["label"="para-group:","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="point"]
"36b4fdfe-9621-4fe4-9169-5855abb0c025" ["label"="P7","style"="filled","fontcolor"="white","fillcolor"="darkseagreen4","shape"="box"]
"1b11da38-86ab-4184-843d-d82de354648f" ["label"="P8","style"="filled","fontcolor"="white","fillcolor"="darkseagreen4","shape"="box"]
"Processing Block: ea6f32dd-0829-473e-b84d-32884384a3a3Processing\\n------------------------\\nMOVE 1234 TO SOMEFRACTION(1).\\nADD 0 TO SOMEFRACTION(1).\\nSUBTRACT 0 FROM SOMEFRACTION(1).\\n/ea6f32dd-0829-473e-b84d-32884384a3a3" ["label"="Processing\\n------------------------\\nMOVE 1234 TO SOMEFRACTION(1).\\nADD 0 TO SOMEFRACTION(1).\\nSUBTRACT 0 FROM SOMEFRACTION(1).\\n","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="box"]
"Processing Block: e45af715-c8ef-4751-885b-7e07a2fda88bProcessing\\n------------------------\\nDISPLAY \\"SOMETEXT = \\" SOMETEXT.\\n/e45af715-c8ef-4751-885b-7e07a2fda88b" ["label"="Processing\\n------------------------\\nDISPLAY \\"SOMETEXT = \\" SOMETEXT.\\n","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="box"]
"2dbfe217-46f5-4c5f-a03d-43a0700a54c5" ["label"="IF(SOMETEXT)=\\"1","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="point"]
"ed0540ab-ade1-4051-8142-0e84e8e410ce" ["label"="IS \\n(SOMETEXT) = \\"12\\" OR \\"13\\"?\\n","style"="filled","fontcolor"="white","fillcolor"="chocolate4","shape"="diamond"]
"98cafab2-f519-4667-9ed9-aac05a759509" ["label"="Yes","style"="filled","fontcolor"="white","fillcolor"="darkgreen","shape"="doublecircle"]
"beadf58c-32ae-4595-9c59-75b26d5ed5aa" ["label"="DISPLAY\\"THAT WORKED\\"","style"="filled","fontcolor"="white","fillcolor"="chocolate4","shape"="point"]
"3ecdbe8e-d31e-4f22-bb06-5b2f7e3ea249" ["label"="No","style"="filled","fontcolor"="white","fillcolor"="red","shape"="doublecircle"]
"08547a3a-f946-48c2-a82a-c213d7e1b76d" ["label"="DISPLAY\\"THAT DIDN'T WORK\\"","style"="filled","fontcolor"="white","fillcolor"="chocolate4","shape"="point"]
"adf8be4f-14da-42a3-b89d-44820159a593" ["label"="para-group:","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="point"]
"20b6917e-c5cb-497d-ac6b-df6ec7e23040" ["label"="P5","style"="filled","fontcolor"="white","fillcolor"="darkseagreen4","shape"="box"]
"50ca56e6-60f7-40e0-a5d2-7c5c448576bd" ["label"="P6","style"="filled","fontcolor"="white","fillcolor"="darkseagreen4","shape"="box"]
"Processing Block: 4362bc93-d9b8-44ef-be15-8cc5919a6ceeProcessing\\n------------------------\\nMOVE 1234 TO SOMEFRACTION(1).\\nADD 0 TO SOMEFRACTION(1).\\nSUBTRACT 0 FROM SOMEFRACTION(1).\\n/4362bc93-d9b8-44ef-be15-8cc5919a6cee" ["label"="Processing\\n------------------------\\nMOVE 1234 TO SOMEFRACTION(1).\\nADD 0 TO SOMEFRACTION(1).\\nSUBTRACT 0 FROM SOMEFRACTION(1).\\n","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="box"]
"Processing Block: 48ec0c02-b004-4196-b47a-2e0ef1f822eaProcessing\\n------------------------\\nDISPLAY \\"SOMETEXT = \\" SOMETEXT.\\n/48ec0c02-b004-4196-b47a-2e0ef1f822ea" ["label"="Processing\\n------------------------\\nDISPLAY \\"SOMETEXT = \\" SOMETEXT.\\n","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="box"]
"0cd0185b-9467-4ec2-842b-74e79bcfacad" ["label"="IF(SOMETEXT)=\\"1","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="point"]
"Processing Block: 8af55611-109f-4df2-aac3-6d7d23e94a5bProcessing\\n------------------------\\nDISPLAY \\"WEIRD STUFF\\".\\n/8af55611-109f-4df2-aac3-6d7d23e94a5b" ["label"="Processing\\n------------------------\\nDISPLAY \\"WEIRD STUFF\\".\\n","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="box"]
"f113cfb2-2bed-4339-a129-e700269d7bd1" ["label"="IS \\n(SOMETEXT) = \\"12\\" OR \\"13\\"?\\n","style"="filled","fontcolor"="white","fillcolor"="chocolate4","shape"="diamond"]
"bb4c3e55-7208-4293-b4f1-d1e072512197" ["label"="Yes","style"="filled","fontcolor"="white","fillcolor"="darkgreen","shape"="doublecircle"]
"84256709-1b5e-4e34-9b5f-1e0e2e5f6cbb" ["label"="DISPLAY\\"THAT WORKED\\"","style"="filled","fontcolor"="white","fillcolor"="chocolate4","shape"="point"]
"fc9b0fba-c5b2-47b6-806d-2d52beea853a" ["label"="No","style"="filled","fontcolor"="white","fillcolor"="red","shape"="doublecircle"]
"54c8cadc-0d4c-4f6e-9650-1800b8c4f7ab" ["label"="DISPLAY\\"THAT DIDN'T WORK\\"","style"="filled","fontcolor"="white","fillcolor"="chocolate4","shape"="point"]
"27061a06-8ba9-4e8c-bffd-011d2d435ea0" ["label"="para-group:","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="point"]
"43c844fa-72a1-4f6f-8ce9-f1cc28815881" ["label"="P2","style"="filled","fontcolor"="white","fillcolor"="darkseagreen4","shape"="box"]
"37975513-cf64-4379-b493-07e1b6f24793" ["label"="P3","style"="filled","fontcolor"="white","fillcolor"="darkseagreen4","shape"="box"]
"86b9dbc7-db0b-4086-aa13-4eb4e47f9611" ["label"="P4","style"="filled","fontcolor"="white","fillcolor"="darkseagreen4","shape"="box"]
"Processing Block: 7a9f25b7-b6ea-4bb8-b539-3235deed096fProcessing\\n------------------------\\nADD SOMETEXT TO SOMETEXT.\\nMOVE 10 TO SOMEFRACTION(1).\\nCOMPUTE SOMETEXT = 2 * SOMETEXT + 1.\\nCOMPUTE SOMETEXT = SOMETEXT / SOMEFRACTION(1).\\n/7a9f25b7-b6ea-4bb8-b539-3235deed096f" ["label"="Processing\\n------------------------\\nADD SOMETEXT TO SOMETEXT.\\nMOVE 10 TO SOMEFRACTION(1).\\nCOMPUTE SOMETEXT = 2 * SOMETEXT + 1.\\nCOMPUTE SOMETEXT = SOMETEXT / SOMEFRACTION(1).\\n","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="box"]
"Processing Block: 9c63c440-2e66-471e-a6c2-6151feafdb34Processing\\n------------------------\\nMOVE \\"0000\\" TO LEVEL-20-B(1 1).\\nMOVE \\"0011\\" TO LEVEL-20-B(1 2).\\nMOVE \\"1100\\" TO LEVEL-20-B(2 1).\\nMOVE \\"1111\\" TO LEVEL-20-B(2 2).\\nDISPLAY \\"LEVEL-20-A = \\" LEVEL-20-A.\\nADD 1 TO 1 GIVING SOMETEXT.\\nDIVIDE 10 INTO SOMETEXT.\\nDISPLAY \\"SOMETEXT XX= \\" SOMETEXT.\\nADD 1 TO 1 GIVING SOMETEXT.\\nSUBTRACT 5 FROM 30 GIVING SOMETEXT.\\nDISPLAY \\"SOMETEXT YY= \\" SOMETEXT.\\nMULTIPLY 2 BY 2 GIVING INVOICE-AMOUNT.\\n/9c63c440-2e66-471e-a6c2-6151feafdb34" ["label"="Processing\\n------------------------\\nMOVE \\"0000\\" TO LEVEL-20-B(1 1).\\nMOVE \\"0011\\" TO LEVEL-20-B(1 2).\\nMOVE \\"1100\\" TO LEVEL-20-B(2 1).\\nMOVE \\"1111\\" TO LEVEL-20-B(2 2).\\nDISPLAY \\"LEVEL-20-A = \\" LEVEL-20-A.\\nADD 1 TO 1 GIVING SOMETEXT.\\nDIVIDE 10 INTO SOMETEXT.\\nDISPLAY \\"SOMETEXT XX= \\" SOMETEXT.\\nADD 1 TO 1 GIVING SOMETEXT.\\nSUBTRACT 5 FROM 30 GIVING SOMETEXT.\\nDISPLAY \\"SOMETEXT YY= \\" SOMETEXT.\\nMULTIPLY 2 BY 2 GIVING INVOICE-AMOUNT.\\n","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="box"]
"Processing Block: 524eb63e-92a0-4300-8324-c764bd517763Processing\\n------------------------\\nADD SOMETEXT, SCALED, 30 TO SOMETHING, RESULT.\\nMOVE SOMETEXT2 TO SOMETEXT2.\\nMOVE 0 TO SOMETEXT2.\\nMOVE \\"ABCD\\" TO LEVEL-10-A(1).\\nMOVE \\"EFGH\\" TO LEVEL-10-A(2).\\nMOVE \\"IJKL\\" TO LEVEL-10-A(3).\\n/524eb63e-92a0-4300-8324-c764bd517763" ["label"="Processing\\n------------------------\\nADD SOMETEXT, SCALED, 30 TO SOMETHING, RESULT.\\nMOVE SOMETEXT2 TO SOMETEXT2.\\nMOVE 0 TO SOMETEXT2.\\nMOVE \\"ABCD\\" TO LEVEL-10-A(1).\\nMOVE \\"EFGH\\" TO LEVEL-10-A(2).\\nMOVE \\"IJKL\\" TO LEVEL-10-A(3).\\n","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="box"]
"c7ac8e46-b283-475e-86bf-d4ccd8b3b6e5" ["label"="para-group:","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="point"]
"27dc2bd7-49f7-494b-81b4-07371c9de4ee" ["label"="P1","style"="filled","fontcolor"="white","fillcolor"="darkseagreen4","shape"="box"]
"f1185750-90fe-4a5a-aebb-3f9a972a6766" ["label"="EVALUATETRUEALS","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="point"]
"51b536fb-c286-47db-b241-f9979e6c882c" ["label"="PERFORMTESTBEFO","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="point"]
"702da7c9-f7d3-4baf-b0b5-f203604a0a67" ["label"="GOTOSECTION-ASE","style"="filled","fontcolor"="black","fillcolor"="wheat","shape"="point"]
"6f219777-6ed8-4c12-bffa-a73e5f2362ea" ["label"="GO TO SECTION-A, SECTION-B, SECTION-B1 DEPENDING ON RESULT","style"="filled","fontcolor"="white","fillcolor"="darkgreen","shape"="invhouse"]
"a3981f57-37a6-4642-ad41-a2836afa4234" ["label"="PERFORM TEST BEFORE VARYING SO","style"="filled","fontcolor"="white","fillcolor"="darkviolet","shape"="cds"]
"4a042d91-6b0c-408e-a4d6-72e5e6b83d97" ["label"="DISPLAY\\"GOING \\"SOME-PART-1\\" AND \\"SOME-PART-2","style"="filled","fontcolor"="white","fillcolor"="chocolate4","shape"="point"]
"7617da71-ca87-4872-9b59-2dbf8f946d5d" ["style"="filled","fontcolor"="black","fillcolor"="white","shape"="underline","label"="TEST BEFORE VARYING SOME-PART-1 FROM 1 BY 1\\n           UNTIL SOME-PART-1 > 10\\n           AFTER SOME-PART-2 FROM 1 BY 1 UNTIL SOME-PART-2 > 10"]
"40237069-fd9f-46a2-ae27-e80b32531cb6" ["label"="EVALUATE","style"="filled","fontcolor"="white","fillcolor"="red4","shape"="diamond"]
"7f132a6a-ad4c-4897-af9e-8804986c5f4d" ["label"="SEARCH\\nSOME-ARRAY","style"="filled","fontcolor"="white","fillcolor"="darkgoldenrod4","shape"="invhouse"]
"95d62c68-9d92-4e8e-9785-620a4d7e6010" ["style"="filled","fontcolor"="white","fillcolor"="deepskyblue4","shape"="note","label"="When\\nSOME-PART-1 > 10"]
"9233caa6-c22e-4a5e-bacd-e600dc4c5489" ["label"="DISPLAY\\"CONDITION 1\\"","style"="filled","fontcolor"="white","fillcolor"="chocolate4","shape"="point"]
"8063f0ba-22a5-4685-a135-c119e891b2b3" ["style"="filled","fontcolor"="white","fillcolor"="deepskyblue4","shape"="note","label"="When\\nSOME-PART-2 <= 10"]
"99b7e4ff-3c0b-4afd-9e7e-956c09279c68" ["label"="DISPLAY\\"CONDITION 2\\"","style"="filled","fontcolor"="white","fillcolor"="chocolate4","shape"="point"]
"a2e403dc-3efd-492f-a4e1-fb1411a1110e"
"2f0b31d2-70db-493e-9c6e-2c1b5373215d"
"a01fdc95-fbb4-4b98-b08d-5fb144465d7a" -> "074de610-9dd6-45d6-8c96-a38d4ef4c0bb" ["style"="dashed","arrowhead"="normal"]
"074de610-9dd6-45d6-8c96-a38d4ef4c0bb" -> "9f2d4205-e870-4dd2-9f27-f772f2eb472e" ["penwidth"="3"]
"9f2d4205-e870-4dd2-9f27-f772f2eb472e" -> "9815f35f-6506-4f93-9557-eb527d806326" ["penwidth"="3"]
"9f2d4205-e870-4dd2-9f27-f772f2eb472e" -> "c7ac8e46-b283-475e-86bf-d4ccd8b3b6e5" ["style"="dashed","arrowhead"="normal"]
"9815f35f-6506-4f93-9557-eb527d806326" -> "76fdd3b8-0490-44dc-b8f4-92d5a1cde642" ["penwidth"="3"]
"9815f35f-6506-4f93-9557-eb527d806326" -> "27061a06-8ba9-4e8c-bffd-011d2d435ea0" ["style"="dashed","arrowhead"="normal"]
"76fdd3b8-0490-44dc-b8f4-92d5a1cde642" -> "4113107a-95cc-4454-b245-60390fac5875" ["penwidth"="3"]
"76fdd3b8-0490-44dc-b8f4-92d5a1cde642" -> "adf8be4f-14da-42a3-b89d-44820159a593" ["style"="dashed","arrowhead"="normal"]
"4113107a-95cc-4454-b245-60390fac5875" -> "d9d5a0d1-04b0-417c-9a37-2de905217aa6" ["penwidth"="3"]
"4113107a-95cc-4454-b245-60390fac5875" -> "d037bbf4-eabb-477c-84ec-bf7dc37e6579" ["style"="dashed","arrowhead"="normal"]
"d9d5a0d1-04b0-417c-9a37-2de905217aa6" -> "f79d7df3-fa45-42fc-9790-eeb538621f34" ["style"="dashed","arrowhead"="normal"]
"f79d7df3-fa45-42fc-9790-eeb538621f34" -> "b74c5262-3736-4c77-83b9-e427e0c5afc1" ["style"="dashed","arrowhead"="normal"]
"b74c5262-3736-4c77-83b9-e427e0c5afc1" -> "db2f7768-f1a3-4e5d-a703-b8cac31a41e1" ["penwidth"="3"]
"b74c5262-3736-4c77-83b9-e427e0c5afc1" -> "Processing Block: 81982782-9662-484a-82c0-07d391e0c491Processing\\n------------------------\\nDISPLAY \\"SOME-UNION-1 = \\" SOME-UNION-1.\\nMOVE 50 TO SOMEFRACTION(1).\\nMOVE \\"ABCD\\" TO SOMETHING.\\nMOVE \\"E\\" TO CONDI.\\n/81982782-9662-484a-82c0-07d391e0c491" ["style"="dashed","arrowhead"="normal"]
"db2f7768-f1a3-4e5d-a703-b8cac31a41e1" -> "94388d97-e553-45d5-a8a0-1ba64a1232c5" ["style"="dashed","arrowhead"="normal"]
"94388d97-e553-45d5-a8a0-1ba64a1232c5" -> "7413e668-3333-4c94-b3df-992267c29e81" ["penwidth"="3"]
"94388d97-e553-45d5-a8a0-1ba64a1232c5" -> "a0bd9989-7a16-4575-aa14-35aaad8a61ae" ["style"="dashed","arrowhead"="normal"]
"7413e668-3333-4c94-b3df-992267c29e81" -> "cf4d94bc-3e47-4807-ad99-fe6eac5a5966" ["penwidth"="3"]
"7413e668-3333-4c94-b3df-992267c29e81" -> "4e859b75-f468-4733-9147-565a902729bb" ["style"="dashed","arrowhead"="normal"]
"cf4d94bc-3e47-4807-ad99-fe6eac5a5966" -> "c83efbe3-3d2e-4ecf-8430-34092b047050" ["style"="dashed","arrowhead"="normal"]
"4e859b75-f468-4733-9147-565a902729bb" -> "8eac8925-5678-4794-a9bf-14f4df732a8a" ["style"="dashed","arrowhead"="normal"]
"8eac8925-5678-4794-a9bf-14f4df732a8a" -> "cae8f4b2-91f8-43d8-9ae1-c3b20e9e3038" ["style"="dashed","arrowhead"="none"]
"a0bd9989-7a16-4575-aa14-35aaad8a61ae" -> "bfc7bc99-274f-4a40-a6e6-28a0664096ab" ["style"="dashed","arrowhead"="normal"]
"bfc7bc99-274f-4a40-a6e6-28a0664096ab" -> "aec2b484-6fa0-480a-b73e-60fff928a17b" ["style"="dashed","arrowhead"="none"]
"d037bbf4-eabb-477c-84ec-bf7dc37e6579" -> "36b4fdfe-9621-4fe4-9169-5855abb0c025" ["style"="dashed","arrowhead"="normal"]
"36b4fdfe-9621-4fe4-9169-5855abb0c025" -> "1b11da38-86ab-4184-843d-d82de354648f" ["penwidth"="3"]
"36b4fdfe-9621-4fe4-9169-5855abb0c025" -> "Processing Block: e45af715-c8ef-4751-885b-7e07a2fda88bProcessing\\n------------------------\\nDISPLAY \\"SOMETEXT = \\" SOMETEXT.\\n/e45af715-c8ef-4751-885b-7e07a2fda88b" ["style"="dashed","arrowhead"="normal"]
"1b11da38-86ab-4184-843d-d82de354648f" -> "Processing Block: ea6f32dd-0829-473e-b84d-32884384a3a3Processing\\n------------------------\\nMOVE 1234 TO SOMEFRACTION(1).\\nADD 0 TO SOMEFRACTION(1).\\nSUBTRACT 0 FROM SOMEFRACTION(1).\\n/ea6f32dd-0829-473e-b84d-32884384a3a3" ["style"="dashed","arrowhead"="normal"]
"Processing Block: e45af715-c8ef-4751-885b-7e07a2fda88bProcessing\\n------------------------\\nDISPLAY \\"SOMETEXT = \\" SOMETEXT.\\n/e45af715-c8ef-4751-885b-7e07a2fda88b" -> "2dbfe217-46f5-4c5f-a03d-43a0700a54c5" ["penwidth"="3"]
"2dbfe217-46f5-4c5f-a03d-43a0700a54c5" -> "ed0540ab-ade1-4051-8142-0e84e8e410ce" ["style"="dashed","arrowhead"="normal"]
"ed0540ab-ade1-4051-8142-0e84e8e410ce" -> "98cafab2-f519-4667-9ed9-aac05a759509" ["style"="dashed","arrowhead"="normal"]
"ed0540ab-ade1-4051-8142-0e84e8e410ce" -> "3ecdbe8e-d31e-4f22-bb06-5b2f7e3ea249" ["style"="dashed","arrowhead"="normal"]
"98cafab2-f519-4667-9ed9-aac05a759509" -> "beadf58c-32ae-4595-9c59-75b26d5ed5aa" ["style"="dashed","arrowhead"="none"]
"3ecdbe8e-d31e-4f22-bb06-5b2f7e3ea249" -> "08547a3a-f946-48c2-a82a-c213d7e1b76d" ["style"="dashed","arrowhead"="none"]
"adf8be4f-14da-42a3-b89d-44820159a593" -> "20b6917e-c5cb-497d-ac6b-df6ec7e23040" ["style"="dashed","arrowhead"="normal"]
"20b6917e-c5cb-497d-ac6b-df6ec7e23040" -> "50ca56e6-60f7-40e0-a5d2-7c5c448576bd" ["penwidth"="3"]
"20b6917e-c5cb-497d-ac6b-df6ec7e23040" -> "Processing Block: 48ec0c02-b004-4196-b47a-2e0ef1f822eaProcessing\\n------------------------\\nDISPLAY \\"SOMETEXT = \\" SOMETEXT.\\n/48ec0c02-b004-4196-b47a-2e0ef1f822ea" ["style"="dashed","arrowhead"="normal"]
"50ca56e6-60f7-40e0-a5d2-7c5c448576bd" -> "Processing Block: 4362bc93-d9b8-44ef-be15-8cc5919a6ceeProcessing\\n------------------------\\nMOVE 1234 TO SOMEFRACTION(1).\\nADD 0 TO SOMEFRACTION(1).\\nSUBTRACT 0 FROM SOMEFRACTION(1).\\n/4362bc93-d9b8-44ef-be15-8cc5919a6cee" ["style"="dashed","arrowhead"="normal"]
"Processing Block: 48ec0c02-b004-4196-b47a-2e0ef1f822eaProcessing\\n------------------------\\nDISPLAY \\"SOMETEXT = \\" SOMETEXT.\\n/48ec0c02-b004-4196-b47a-2e0ef1f822ea" -> "0cd0185b-9467-4ec2-842b-74e79bcfacad" ["penwidth"="3"]
"0cd0185b-9467-4ec2-842b-74e79bcfacad" -> "Processing Block: 8af55611-109f-4df2-aac3-6d7d23e94a5bProcessing\\n------------------------\\nDISPLAY \\"WEIRD STUFF\\".\\n/8af55611-109f-4df2-aac3-6d7d23e94a5b" ["penwidth"="3"]
"0cd0185b-9467-4ec2-842b-74e79bcfacad" -> "f113cfb2-2bed-4339-a129-e700269d7bd1" ["style"="dashed","arrowhead"="normal"]
"f113cfb2-2bed-4339-a129-e700269d7bd1" -> "bb4c3e55-7208-4293-b4f1-d1e072512197" ["style"="dashed","arrowhead"="normal"]
"f113cfb2-2bed-4339-a129-e700269d7bd1" -> "fc9b0fba-c5b2-47b6-806d-2d52beea853a" ["style"="dashed","arrowhead"="normal"]
"bb4c3e55-7208-4293-b4f1-d1e072512197" -> "84256709-1b5e-4e34-9b5f-1e0e2e5f6cbb" ["style"="dashed","arrowhead"="none"]
"fc9b0fba-c5b2-47b6-806d-2d52beea853a" -> "54c8cadc-0d4c-4f6e-9650-1800b8c4f7ab" ["style"="dashed","arrowhead"="none"]
"27061a06-8ba9-4e8c-bffd-011d2d435ea0" -> "43c844fa-72a1-4f6f-8ce9-f1cc28815881" ["style"="dashed","arrowhead"="normal"]
"43c844fa-72a1-4f6f-8ce9-f1cc28815881" -> "37975513-cf64-4379-b493-07e1b6f24793" ["penwidth"="3"]
"43c844fa-72a1-4f6f-8ce9-f1cc28815881" -> "Processing Block: 524eb63e-92a0-4300-8324-c764bd517763Processing\\n------------------------\\nADD SOMETEXT, SCALED, 30 TO SOMETHING, RESULT.\\nMOVE SOMETEXT2 TO SOMETEXT2.\\nMOVE 0 TO SOMETEXT2.\\nMOVE \\"ABCD\\" TO LEVEL-10-A(1).\\nMOVE \\"EFGH\\" TO LEVEL-10-A(2).\\nMOVE \\"IJKL\\" TO LEVEL-10-A(3).\\n/524eb63e-92a0-4300-8324-c764bd517763" ["style"="dashed","arrowhead"="normal"]
"37975513-cf64-4379-b493-07e1b6f24793" -> "86b9dbc7-db0b-4086-aa13-4eb4e47f9611" ["penwidth"="3"]
"37975513-cf64-4379-b493-07e1b6f24793" -> "Processing Block: 9c63c440-2e66-471e-a6c2-6151feafdb34Processing\\n------------------------\\nMOVE \\"0000\\" TO LEVEL-20-B(1 1).\\nMOVE \\"0011\\" TO LEVEL-20-B(1 2).\\nMOVE \\"1100\\" TO LEVEL-20-B(2 1).\\nMOVE \\"1111\\" TO LEVEL-20-B(2 2).\\nDISPLAY \\"LEVEL-20-A = \\" LEVEL-20-A.\\nADD 1 TO 1 GIVING SOMETEXT.\\nDIVIDE 10 INTO SOMETEXT.\\nDISPLAY \\"SOMETEXT XX= \\" SOMETEXT.\\nADD 1 TO 1 GIVING SOMETEXT.\\nSUBTRACT 5 FROM 30 GIVING SOMETEXT.\\nDISPLAY \\"SOMETEXT YY= \\" SOMETEXT.\\nMULTIPLY 2 BY 2 GIVING INVOICE-AMOUNT.\\n/9c63c440-2e66-471e-a6c2-6151feafdb34" ["style"="dashed","arrowhead"="normal"]
"86b9dbc7-db0b-4086-aa13-4eb4e47f9611" -> "Processing Block: 7a9f25b7-b6ea-4bb8-b539-3235deed096fProcessing\\n------------------------\\nADD SOMETEXT TO SOMETEXT.\\nMOVE 10 TO SOMEFRACTION(1).\\nCOMPUTE SOMETEXT = 2 * SOMETEXT + 1.\\nCOMPUTE SOMETEXT = SOMETEXT / SOMEFRACTION(1).\\n/7a9f25b7-b6ea-4bb8-b539-3235deed096f" ["style"="dashed","arrowhead"="normal"]
"c7ac8e46-b283-475e-86bf-d4ccd8b3b6e5" -> "27dc2bd7-49f7-494b-81b4-07371c9de4ee" ["style"="dashed","arrowhead"="normal"]
"27dc2bd7-49f7-494b-81b4-07371c9de4ee" -> "f1185750-90fe-4a5a-aebb-3f9a972a6766" ["style"="dashed","arrowhead"="normal"]
"f1185750-90fe-4a5a-aebb-3f9a972a6766" -> "51b536fb-c286-47db-b241-f9979e6c882c" ["penwidth"="3"]
"f1185750-90fe-4a5a-aebb-3f9a972a6766" -> "40237069-fd9f-46a2-ae27-e80b32531cb6" ["style"="dashed","arrowhead"="normal"]
"51b536fb-c286-47db-b241-f9979e6c882c" -> "702da7c9-f7d3-4baf-b0b5-f203604a0a67" ["penwidth"="3"]
"51b536fb-c286-47db-b241-f9979e6c882c" -> "a3981f57-37a6-4642-ad41-a2836afa4234" ["style"="dashed","arrowhead"="normal"]
"702da7c9-f7d3-4baf-b0b5-f203604a0a67" -> "6f219777-6ed8-4c12-bffa-a73e5f2362ea" ["style"="dashed","arrowhead"="normal"]
"6f219777-6ed8-4c12-bffa-a73e5f2362ea" -> "9815f35f-6506-4f93-9557-eb527d806326" ["style"="bold","color"="blueviolet"]
"6f219777-6ed8-4c12-bffa-a73e5f2362ea" -> "76fdd3b8-0490-44dc-b8f4-92d5a1cde642" ["style"="bold","color"="blueviolet"]
"6f219777-6ed8-4c12-bffa-a73e5f2362ea" -> "4113107a-95cc-4454-b245-60390fac5875" ["style"="bold","color"="blueviolet"]
"a3981f57-37a6-4642-ad41-a2836afa4234" -> "4a042d91-6b0c-408e-a4d6-72e5e6b83d97" ["style"="dashed","arrowhead"="normal"]
"a3981f57-37a6-4642-ad41-a2836afa4234" -> "7617da71-ca87-4872-9b59-2dbf8f946d5d" ["style"="bold","color"="blueviolet"]
"40237069-fd9f-46a2-ae27-e80b32531cb6" -> "7f132a6a-ad4c-4897-af9e-8804986c5f4d" ["penwidth"="3"]
"7f132a6a-ad4c-4897-af9e-8804986c5f4d" -> "95d62c68-9d92-4e8e-9785-620a4d7e6010" ["style"="dashed","arrowhead"="normal"]
"7f132a6a-ad4c-4897-af9e-8804986c5f4d" -> "8063f0ba-22a5-4685-a135-c119e891b2b3" ["style"="dashed","arrowhead"="normal"]
"95d62c68-9d92-4e8e-9785-620a4d7e6010" -> "9233caa6-c22e-4a5e-bacd-e600dc4c5489" ["style"="dashed","arrowhead"="normal"]
"8063f0ba-22a5-4685-a135-c119e891b2b3" -> "99b7e4ff-3c0b-4afd-9e7e-956c09279c68" ["style"="dashed","arrowhead"="normal"]
}
      `, {
        graphAttributes: {
          splines: "ortho"
        }
      });
      document.getElementById("flowchart").appendChild(svg);
    });
  },
  methods: {
    navigateToCytoNode(data) {
      // console.log("Parent notified");
      // console.log(data.id);
      this.centerNode = flip(data.id, this.centerNode);
    },
    updateNodeDetails(data) {
      this.nodeDetails = data;
    },
    receiveLoadIntermediateASTEvent(data) {
      // console.log("Received event");
      // console.log(data);
      this.getIRWithID(data);
    },
    receiveLoadIntermediateCFGEvent(data) {
      // console.log("Received event");
      // console.log(data);
      this.getCFGWithID(data);
    },
    receiveLoadFlowModelEvent(data) {
      // console.log("Received event");
      // console.log(data);
      this.getFlowModelWithID(data);
    },
    testPing() {
      const self = this;
      axios.get("/api/heartbeat")
          .then(response => {
            console.log(response);
            this.heartbeatResult = response.data;
          })
          .catch(function (err) {
            self.heartbeatResult = "FAIL";
            console.log(err);
          });
    },
    getIRWithID(id) {
      axios.get("/api/ir-ast/" + id)
          .then(response => {
            this.irAST = response.data.ast;
            this.t1t2Result = null;
          })
          .catch(function (err) {
            console.log("There was an error: ");
            console.log(err);
          });
    },
    getIR() {
      this.getIRWithID(1);
    },
    getCFG() {
      this.getCFGWithID(1);
    },
    async getFlowModelWithID(id) {
      axios.get("/api/flow-model/" + id)
          .then(response => {
            console.log(response);
            this.flowModel = unifiedModelToDigraph(response.data.body);
            return response.data.body;
          });
    },
    async getCFGWithID(id) {
      const cfgPromise = axios.get("/api/ir-cfg/" + id)
          .then(response => {
            console.log(response);
            this.irCFG = response.data.cfg;
            return response.data.cfg;
          });
      const loopBodyPromise = axios.get("/api/ir-cfg/" + id + "/loop-body")
          .then(response => {
            console.log(response);
            return response.data;
          });
      const t1t2Promise = axios.get("/api/ir-cfg/" + id + "/t1-t2")
          .then(response => {
            console.log(response);
            return response.data;
          });

      try {
        const [irCFG, loopBodies, t1t2Result] = await Promise.all([cfgPromise, loopBodyPromise, t1t2Promise]);
        this.irCFG = irCFG;
        this.loopBodies = loopBodies;
        this.t1t2Result = t1t2Result;
      } catch (e) {
        console.log("There was an error: ");
        console.log(e);
      }
    }
  },
  computed: {}
}
</script>

<template>
  <div id="top-panel">
    <img alt="Cobol-REKT logo" src="./assets/cobol-rekt-banner.png" style="width: 30%; height: auto">
    <div class="functions">
      <button @click="testPing" class="function-button">Test Ping</button>
      <button class="function-button">Flowchart</button>
      <button @click="getIR" class="function-button">Intermediate Form</button>
      <button @click="getCFG" class="function-button">Control Flowgraph</button>
      <button class="function-button">Capability Mapping</button>
      <button class="function-button">Node Summarisation</button>
      <button class="function-button">T1/T2 Reducibility</button>
      <button class="function-button">SCCs</button>
      <button class="function-button">Loop Bodies</button>
      <button class="function-button">Trace Dependencies</button>
      <button class="function-button">Code Patterns</button>
      <button class="function-button">Eliminate GO TOs</button>
    </div>
  </div>

  <HelloWorld/>
  <div>Last Ping result is: {{ heartbeatResult }}</div>
  <div class="main-panel">
    <CodePane :ir-a-s-t="irAST" style="grid-area: 1 / 1 / 3 / 2" @sourceNodeClicked="navigateToCytoNode"/>
    <GraphView
        :intermediate-cfg-digraph="irCFG"
        :intermediate-ast="irAST"
        :loopBodies="loopBodies"
        :t1t2Result="t1t2Result"
        :center-node="centerNode"
        :flow-model="flowModel"
        @node-details-changed="updateNodeDetails"
        style="grid-area: 1 / 2 / 3 / 3"
    />
    <InfoPane :node-details="this.nodeDetails"
              style="grid-area: 1 / 3 / 2 / 4"/>
    <ProjectsView @load-ir-ast="receiveLoadIntermediateASTEvent"
                  @load-ir-cfg="receiveLoadIntermediateCFGEvent"
                  @load-flow-model="receiveLoadFlowModelEvent"
                  style="grid-area: 2 / 3 / 3 / 4"
    />
  </div>
  LOL
  <div id="flowchart" style="height: 300px; border: 1px solid;">

  </div>
</template>

<style>
#app {
  font-family: "Roboto Light", "Helvetica Neue", Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  color: #2c3e50;
  margin-top: 10px;
}

.code {
  font-family: "Andale Mono", sans-serif;
  background-color: azure;
}

.readonly-code {
  font-family: "Andale Mono", sans-serif;
  background-color: azure;
  text-align: left;
  white-space: pre-wrap;
}

.code-pane {
  height: 100%;
  overflow-y: auto;
}

#node-details {
  overflow-y: auto;
  background-color: azure;
}

.functions {
  display: grid;
  grid-template-columns: repeat(6, 1fr); /* 6 columns for each button */
  grid-template-rows: 1fr 1fr; /* 2 rows with equal size */
  gap: 5px; /* 5px gap between buttons */
}

.main-panel {
  height: 600px;
  width: 99%;
  display: grid;
  grid-template-columns: 42% 36% 22%;
  grid-template-rows: 50% 50%;
  gap: 5px;
}

#top-panel {
  display: flex;
  gap: 10px;
}

.pane-heading {
  background-color: #5b6e83;
  color: white;
  padding-left: 10px;
  padding-top: 5px;
  padding-bottom: 5px;
  border-top-left-radius: 10px;
  border-top-right-radius: 10px;
}

.headered-pane {
  border: 1px solid #c3d2e7;
  display: flex;
  flex-flow: column nowrap;
  border-radius: 10px;
  overflow: hidden;
}

.function-button {
  background-color: #5b6e83;
  border-radius: 8px;
  border-style: none;
  box-sizing: border-box;
  color: #FFFFFF;
  cursor: pointer;
  display: inline-block;
  font-family: "Haas Grot Text R Web", "Helvetica Neue", Helvetica, Arial, sans-serif;
  font-size: 14px;
  font-weight: 500;
  height: 40px;
  list-style: none;
  margin: 0;
  outline: none;
  padding: 10px 16px;
  position: relative;
  text-align: center;
  text-decoration: none;
  vertical-align: baseline;
  user-select: none;
  -webkit-user-select: none;
  touch-action: manipulation;
}

.function-button:hover {
  background-color: #42b983;
}

.function-button:active {
  transform: scale(0.98);
}
</style>
