<script>
import {ref} from "vue";
import HelloWorld from "@/components/HelloWorld.vue";
import {TestAstNode} from "@/ts/TestAstNode";
import axios from "axios";
import UiIntermediateAstNode from "@/components/UiIntermediateAstNode.vue";
import ProjectsView from "@/components/ProjectsView.vue";
import GraphView from "@/components/GraphView.vue";

export default {
  name: 'App',
  components: {
    GraphView,
    UiIntermediateAstNode,
    HelloWorld,
    ProjectsView
  },
  setup() {
    const codeArea = ref("Here's some code");
    return {codeArea};
  },
  data() {
    const testGraph = new TestAstNode("A1", "TOP",
        [
          new TestAstNode("AA1", "BOTTOM", [])
        ]
    );
    return {testGraph, heartbeatResult: "UNKNOWN", irAST: null, cy: null, nodeDetails: null};
  },
  mounted() {
    this.drawGraph();
  },
  methods: {
    updateNodeDetails(data) {
      this.nodeDetails = data;
    },
    receiveLoadIntermediateASTEvent(data) {
      console.log("Received event");
      console.log(data);
      this.getIRWithID(data);
    },
    testPing() {
      axios.get("/api/heartbeat")
          .then(response => {
            console.log(response);
            this.heartbeatResult = response.data;
          })
    },
    getIRWithID(id) {
      axios.get("/api/ir-ast/" + id)
          .then(response => {
            console.log(response);
            console.log(response.data);
            this.irAST = response.data.ast;
          });
    },
    getIR() {
      this.getIRWithID(4);
    },
    recalculatedNodes(current) {
      const currentGraphNodes = [{data: current}];
      if (current.childTranspilerNodes.length == 0) {
        return currentGraphNodes;
      }
      return currentGraphNodes.concat(current.childTranspilerNodes.flatMap(e => this.recalculatedNodes(e)));
    },
    recalculatedEdges(current, thread) {
      const parentNode = thread.length === 0 ? null : thread.at(-1);
      const myEdges = parentNode == null ? [] : [{
        data: {
          id: current.id + parentNode.id,
          source: parentNode.id,
          target: current.id
        }
      }];
      if (current.childTranspilerNodes.length == 0) {
        // console.log("WAS EMPTY");
        return myEdges;
      }
      return myEdges.concat(current.childTranspilerNodes.flatMap(e => this.recalculatedEdges(e, thread.concat(current))));
    },
    drawGraph() {
    }
  },
  computed: {
    irTreePopulated() {
      return this.irAST != null;
    }
  }
}

</script>

<template>
  <div id="top-panel">
    <img alt="Cobol-REKT logo" src="./assets/cobol-rekt-banner.png" style="width: 30%; height: auto">
    <div class="functions">
      <button @click="testPing">Test Ping</button>
      <button>Flowchart</button>
      <button @click="getIR">Intermediate Representation</button>
      <button>Control Flowgraph</button>
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

  <HelloWorld header="Welcome to this amazing app"/>
  <div>Last Ping result is: {{ heartbeatResult }}</div>
  <div class="main-panel">
    <div id="code-view">
      <h3>Intermediate Form Source</h3>
      <div class="readonly-code ir-window">
        <UiIntermediateAstNode :node="irAST" :depth="0" v-if="irTreePopulated"/>
      </div>
    </div>
    <GraphView :graph-model="irAST" @node-details-changed="updateNodeDetails"/>
    <div style="display: flex; flex-direction: column;">
      <div id="node-details-pane">
        <h3>Node Data</h3>
        <div id="node-details">
          {{ this.nodeDetails }}

        </div>
      </div>
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

.ir-window {
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

#cyto {
  height: 600px;
  width: 600px;
  background-color: azure;
  border: 1px solid;
  position: relative;
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
