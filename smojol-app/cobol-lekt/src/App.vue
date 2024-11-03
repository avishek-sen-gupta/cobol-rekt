<script>
import HelloWorld from "@/components/HelloWorld.vue";
import axios from "axios";
import ProjectsView from "@/components/ProjectsView.vue";
import GraphView from "@/components/GraphView.vue";
import InfoPane from "@/components/InfoPane.vue";
import CodePane from "@/components/CodePane.vue";
import {flip} from "@/ts/FlippableId";


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
    return {heartbeatResult: "UNKNOWN", irAST: null, irCFG: null, nodeDetails: null, centerNode: null};
  },
  mounted() {
  },
  methods: {
    navigateToCytoNode(data) {
      console.log("Parent notified");
      console.log(data.id);
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
          })
          .catch(function (err) {
            console.log("There was an error: ");
            console.log(err);
          });
    },
    getIR() {
      this.getIRWithID(4);
    },
    getCFG() {
      this.getCFGWithID(1);
    },
    getCFGWithID(id) {
      axios.get("/api/ir-cfg/" + id)
          .then(response => {
            console.log(response);
            this.irCFG = response.data.cfg;
          })
          .catch(function (err) {
            console.log("There was an error: ");
            console.log(err);
          });
    }
  },
  computed: {}
}
</script>

<template>
  <div id="top-panel">
    <img alt="Cobol-REKT logo" src="./assets/cobol-rekt-banner.png" style="width: 30%; height: auto">
    <div class="functions">
<!--      <button class="mdl-button mdl-js-button mdl-button&#45;&#45;raised mdl-js-ripple-effect mdl-button&#45;&#45;accent">-->
<!--        Test Button-->
<!--      </button>-->
<!--      <button class="mdl-button mdl-js-button mdl-button&#45;&#45;fab mdl-button&#45;&#45;colored">-->
<!--        <i class="material-icons">add</i>-->
<!--      </button>-->
      <button @click="testPing" class="mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored wrapped-text">Test Ping</button>
      <button class="mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored wrapped-text">Flowchart</button>
      <button @click="getIR" class="mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored wrapped-text">Intermediate Representation</button>
      <button @click="getCFG" class="mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored wrapped-text">Control Flowgraph</button>
      <button class="mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored wrapped-text">Capability Mapping</button>
      <button class="mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored wrapped-text">Node Summarisation</button>
      <button class="mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored wrapped-text">T1/T2 Reducibility</button>
      <button class="mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored wrapped-text">Strongly Connected Components</button>
      <button class="mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored wrapped-text">Loop Bodies</button>
      <button class="mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored wrapped-text">Trace Dependencies</button>
      <button class="mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored wrapped-text">Code Patterns</button>
      <button class="mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored wrapped-text">Eliminate GO TOs</button>
    </div>
  </div>

  <HelloWorld/>
  <div>Last Ping result is: {{ heartbeatResult }}</div>
  <div class="main-panel">
    <CodePane :ir-a-s-t="irAST" style="grid-area: 1 / 1 / 3 / 2" @sourceNodeClicked="navigateToCytoNode"/>
    <GraphView
        :digraph-model="irCFG"
        :tree-model="irAST"
        :center-node="centerNode"
        @node-details-changed="updateNodeDetails"
        style="grid-area: 1 / 2 / 3 / 3"
    />
    <InfoPane :node-details="this.nodeDetails"
              style="grid-area: 1 / 3 / 2 / 4"/>
    <ProjectsView @load-ir-ast="receiveLoadIntermediateASTEvent"
                  @load-ir-cfg="receiveLoadIntermediateCFGEvent"
                  style="grid-area: 2 / 3 / 3 / 4"
    />
  </div>
</template>

<style>
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
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
  overflow-y: scroll;
}

#node-details {
  overflow-y: scroll;
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
  font-size: 1.17em;
  font-weight: bold;
  background-color: darkblue;
  color: white;
  padding-left: 10px;
}

.headered-pane {
  border: 1px solid darkslateblue;
  display: flex;
  flex-flow: column nowrap;
}

.wrapped-text {
  font-size: 12px;
  overflow-x: hidden !important;
}
</style>
