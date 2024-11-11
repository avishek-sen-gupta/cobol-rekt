<script lang="ts">
import HelloWorld from "@/components/HelloWorld.vue";
import axios from "axios";
import ProjectsView from "@/components/ProjectsView.vue";
import GraphView from "@/components/GraphView.vue";
import InfoPane from "@/components/InfoPane.vue";
import CodePane from "@/components/CodePane.vue";
import {flip, MutableCenter} from "@/ts/FlippableId";
import {unifiedModelToDigraph} from "@/ts/UnifiedFlowModel";
import ImageView from "@/components/ImageView.vue";
import {defineComponent, ref} from "vue";
import {Digraph} from "./ts/Digraph";

export default defineComponent({
      name: 'App',
      components: {
        CodePane,
        InfoPane,
        GraphView,
        HelloWorld,
        ProjectsView,
        ImageView
      },
      setup() {
        const flowModel = ref<Digraph | null>(null);
        const centerNode = ref<MutableCenter | null>(null);
        const nodeDetails = ref<any | null>(null);
        const source = ref<any | null>(null);
        return {flowModel, centerNode, nodeDetails, source};
      },
      data() {
        return {
          heartbeatResult: "UNKNOWN",
          irAST: null,
          irCFG: null,
          // nodeDetails: null,
          // centerNode: null,
          loopBodies: [],
          t1t2Result: null,
          // flowModel: null,
          flowchart: `digraph {}`,
        };
      },
      mounted() {
      },
      methods: {
        navigateToCytoNode(data: { id: string, [key: string]: any }) {
          // console.log("Parent notified");
          // console.log(data.id);
          this.centerNode = flip(data.id, this.centerNode!);
        },
        updateNodeDetails(data: any) {
          this.nodeDetails = data;
        },
        receiveLoadIntermediateASTEvent(data: Number) {
          // console.log("Received event");
          // console.log(data);
          this.getIRWithID(data);
        },
        receiveLoadRawASTEvent(data: Number) {
          // console.log("Received event");
          // console.log(data);
          this.getRawASTWithID(data);
        },
        receiveLoadIntermediateCFGEvent(data: Number) {
          // console.log("Received event");
          // console.log(data);
          this.getCFGWithID(data);
        },
        receiveLoadFlowModelEvent(data: Number) {
          // console.log("Received event");
          // console.log(data);
          this.getFlowModelWithID(data);
        },
        receiveLoadFlowchartEvent(data: Number) {
          // console.log("Received event");
          // console.log(data);
          this.getFlowchartWithID(data);
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
        getIRWithID(id: Number) {
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
        getRawASTWithID(id: Number) {
          axios.get("/api/raw-ast/" + id)
              .then(response => {
                this.t1t2Result = null;
                this.source = response.data;
                console.log("GOT SOURCE")
                console.log(this.source);
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
        async getFlowModelWithID(id: Number) {
          axios.get("/api/flow-model/" + id)
              .then(response => {
                console.log(response);
                this.flowModel = unifiedModelToDigraph(response.data.body);
                return response.data.body;
              });
        },
        async getFlowchartWithID(id: Number) {
          axios.get("/api/flowchart/" + id)
              .then(response => {
                console.log(response);
                this.flowchart = response.data.markup;
                return response.data.body;
              });
        },
        async getCFGWithID(id: Number) {
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
)</script>

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
    <CodePane :ir-a-s-t="irAST"
              style="grid-area: 1 / 1 / 3 / 2" @sourceNodeClicked="navigateToCytoNode"/>
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
    <InfoPane :node-details="nodeDetails"
              style="grid-area: 1 / 3 / 2 / 4"/>
    <ProjectsView @load-ir-ast="receiveLoadIntermediateASTEvent"
                  @load-ir-cfg="receiveLoadIntermediateCFGEvent"
                  @load-flow-model="receiveLoadFlowModelEvent"
                  @load-flowchart="receiveLoadFlowchartEvent"
                  @load-raw-ast="receiveLoadRawASTEvent"
                  style="grid-area: 2 / 3 / 3 / 4"
    />
  </div>
  <div style="padding-top: 20px;">
    <ImageView :flowchart="flowchart"></ImageView>
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

.clt, .clt ul, .clt li {
  position: relative;
}

.clt ul {
  list-style: none;
  padding-left: 32px;
}

.clt li::before, .clt li::after {
  content: "";
  position: absolute;
  left: -12px;
}

.clt li::before {
  border-top: 1px solid #000;
  top: 9px;
  width: 8px;
  height: 0;
}

.clt li::after {
  border-left: 1px solid #000;
  height: 100%;
  width: 0px;
  top: 2px;
}

.clt ul > li:last-child::after {
  height: 8px;
}

.clickable-item {
  font-size: 1.8em;
  padding: 0;
  border: none;
  background: none;
}

.hoverable:hover {
  background-color: indigo;
  color: white;
}
</style>
