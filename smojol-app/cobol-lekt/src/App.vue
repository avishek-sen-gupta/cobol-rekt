<script>
import {ref} from "vue";
import cytoscape from 'cytoscape';
import cydagre from "cytoscape-dagre";
import HelloWorld from "@/components/HelloWorld.vue";
import {TestAstNode} from "@/ts/TestAstNode";
import axios from "axios";
import UiIntermediateAstNode from "@/components/UiIntermediateAstNode.vue";
import ProjectsView from "@/components/ProjectsView.vue";

export default {
  name: 'App',
  components: {
    UiIntermediateAstNode,
    HelloWorld,
    ProjectsView
  },
  setup() {
    const codeArea = ref("Here's some code");

    function updateText() {
      codeArea.value = "BLAH BLAH";
    }

    const graph = ref({
      id: "A",
      children: [
        {
          id: "A1",
          children: [
            {
              id: "AA1",
              children: []
            },
            {
              id: "AA2",
              children: []
            }
          ]
        },
        {
          id: "A2",
          children: []
        }
      ]
    });
    console.log(graph);
    return {codeArea, updateText: updateText, graph};
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
    testPing() {
      axios.get("/api/heartbeat")
          .then(response => {
            console.log(response);
            this.heartbeatResult = response.data;
          })
    },
    getIR() {
      axios.get("/api/ir-ast")
          .then(response => {
            console.log(response);
            console.log(response.data);
            this.irAST = response.data;
            const cytoNodes = this.recalculatedNodes(this.irAST);
            const cytoEdges = this.recalculatedEdges(this.irAST, []);
            console.log("PRINTTING NODES");
            console.log(cytoNodes);
            cytoEdges.forEach(edge => console.log(edge));
            cydagre(cytoscape);
            this.cy = cytoscape({
              container: document.getElementById("cyto"),
              elements: cytoNodes.concat(cytoEdges),
              style: [ // the stylesheet for the graph
                {
                  selector: 'node',
                  style: {
                    'background-color': '#075',
                    'label': 'data(id)'
                  }
                },
                {
                  selector: 'edge',
                  style: {
                    'width': 3,
                    'line-color': '#ccc',
                    'target-arrow-color': '#ccc',
                    'target-arrow-shape': 'triangle',
                    'curve-style': 'bezier'
                  }
                }
              ],

              layout: {
                name: 'breadthfirst',
                directed: true
              }
            });
            this.cy.on('select', 'node', (event) => {
              const node = event.target;
              // console.log(`Node selected: ${node.target.id}`);
              const nodeData = node.data();
              console.log(nodeData);
              this.nodeDetails = {
                id: nodeData.id,
                nodeType: nodeData.nodeType,
              };
            });
            this.cy.center();

          });
    },
    recalculatedNodes(current) {
      // console.log(current);
      // console.log("CHILDREN=");
      // console.log(current.childTranspilerNodes.length);
      const currentGraphNodes = [{data: current}];
      if (current.childTranspilerNodes.length == 0) {
        // console.log("WAS EMPTY");
        return currentGraphNodes;
      }
      return currentGraphNodes.concat(current.childTranspilerNodes.flatMap(e => this.recalculatedNodes(e)));
    },
    recalculatedEdges(current, thread) {
      // console.log(current);
      // console.log("CHILDREN=");
      // console.log(current.childTranspilerNodes.length);
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
      console.log("Checking IR pop");
      console.log(this.irAST);
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
    <div id="graph-view">
      <h3>Graph View</h3>
      <div id="cyto" class="cyto"></div>
    </div>
    <div style="display: flex; flex-direction: column;">
      <div id="node-details-pane">
        <h3>Node Data</h3>
        <div id="node-details">
          {{ this.nodeDetails }}

        </div>
      </div>
      <ProjectsView/>
    </div>
  </div>
<!--  <textarea v-model="codeArea" rows="10" columns="10" class="code"/>-->
<!--  <button @click="updateText">Le Button</button>-->
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
</style>
