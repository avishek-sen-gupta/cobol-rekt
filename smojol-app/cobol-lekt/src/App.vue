<script>
import {ref} from "vue";
import cytoscape from 'cytoscape';
import cydagre from "cytoscape-dagre";
import UiAstNode from "@/components/UiAstNode.vue";
import HelloWorld from "@/components/HelloWorld.vue";
import {TestAstNode} from "@/ts/TestAstNode";
import axios from "axios";

export default {
  name: 'App',
  components: {
    HelloWorld,
    UiAstNode
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
    return {testGraph, heartbeatResult: null};
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
    drawGraph() {
      cydagre(cytoscape);
      const cy = cytoscape({
        container: document.getElementById('cyto'),
        elements: [ // list of graph elements to start with
          { // node a
            data: {id: 'a'}
          },
          { // node b
            data: {id: 'b'}
          },
          { // node b
            data: {id: 'c'}
          },
          { // edge ab
            data: {id: 'ab', source: 'a', target: 'b'},
          },
          { // edge ac
            data: {id: 'ac', source: 'a', target: 'c'}
          }
        ],
        style: [ // the stylesheet for the graph
          {
            selector: 'node',
            style: {
              'background-color': '#666',
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
      cy.center();
      console.log("DONE " + cy);
    }
  }
}

</script>

<template>
  <img alt="Cobol-REKT logo" src="./assets/cobol-rekt-banner.png">
  <HelloWorld header="Welcome to this amazing app"/>
  <div>
    <button @click="testPing">Test Ping</button>
    <button>Flowchart</button>
    <button>Intermediate Representation</button>
    <button>Control Flowgraph</button>
    <button>Configure/Run Task(s)</button>
  </div>
  <div>Ping result is MODIFIED: {{heartbeatResult}}</div>
  <div class="ir-box">
    <UiAstNode :node="testGraph"/>
  </div>
  <h3>Intermediate representation</h3>
  <div class="readonly-code">What {{ codeArea }}</div>
  <textarea v-model="codeArea" rows="10" columns="10" class="code"/>
  <button @click="updateText">Le Button</button>
</template>


<style>
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  margin-top: 60px;
}

.code {
  font-family: "Courier New", sans-serif;
  background-color: azure;
}

.readonly-code, .ir-box {
  font-family: "Courier New", sans-serif;
  background-color: azure;
  border: 1px solid azure;
  text-align: left;
}

#cyto {
  height: 600px;
  width: 600px;
  background-color: azure;
}
</style>
