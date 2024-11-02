<script>
import HelloWorld from "@/components/HelloWorld.vue";
import {TestAstNode} from "@/ts/TestAstNode";
import axios from "axios";
import ProjectsView from "@/components/ProjectsView.vue";
import GraphView from "@/components/GraphView.vue";
import InfoPane from "@/components/InfoPane.vue";
import CodePane from "@/components/CodePane.vue";

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
    new TestAstNode("A1", "TOP",
        [
          new TestAstNode("AA1", "BOTTOM", [])
        ]
    );
    return {heartbeatResult: "UNKNOWN", irAST: null, irCFG: null, nodeDetails: null};
  }
  ,
  mounted() {
  },
  methods: {
    updateNodeDetails(data) {
      this.nodeDetails = data;
    },
    receiveLoadIntermediateASTEvent(data) {
      // console.log("Received event");
      // console.log(data);
      this.getIRWithID(data);
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
      axios.get("/api/ir-cfg")
          .then(response => {
            console.log(response);
            this.irCFG = response.data;
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
      <button @click="testPing">Test Ping</button>
      <button>Flowchart</button>
      <button @click="getIR">Intermediate Representation</button>
      <button @click="getCFG">Control Flowgraph</button>
      <button>Configure/Run Task(s)</button>
      <button>Capability Mapping</button>
      <button>Node Summarisation</button>
      <button>T1/T2 Reducibility</button>
      <button>Strongly Connected Components</button>
      <button>Identify Loop Bodies</button>
      <button>Trace Program Dependencies</button>
      <button>Code Patterns</button>
      <button>Eliminate GO TOs</button>
    </div>
  </div>

  <HelloWorld/>
  <div>Last Ping result is: {{ heartbeatResult }}</div>
  <div class="main-panel">
    <CodePane :ir-a-s-t="irAST"/>
    <GraphView :digraph-model="irCFG" :tree-model="irAST" @node-details-changed="updateNodeDetails"/>
    <div style="display: flex; flex-direction: column;">
      <InfoPane :node-details="this.nodeDetails"/>
      <ProjectsView @load-ir-ast="receiveLoadIntermediateASTEvent"/>
    </div>
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
  border: 1px solid azure;
  text-align: left;
  white-space: pre-wrap;
}

.code-pane {
  width: 700px;
  height: 600px;
  overflow-y: scroll;
  border: 1px solid;
}

#node-details {
  width: 350px;
  height: 250px;
  overflow-y: scroll;
  border: 1px solid;
  background-color: azure;
}

.functions {
  display: flex;
  width: 1000px;
  height: 100px;
  flex-wrap: wrap;
  gap: 5px;
}

.main-panel {
  display: flex;
  gap: 10px;
}

#top-panel {
  display: flex;
  gap: 10px;
}

.pane-heading {
  font-size: 1.17em;
  font-weight: bold;
  margin-block-start: 1em;
  margin-block-end: 1em;
}
</style>
