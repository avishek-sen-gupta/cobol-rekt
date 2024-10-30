<script>
import {ref} from "vue";
import cytoscape from 'cytoscape';
import cydagre from "cytoscape-dagre";
import UiAstNode from "@/components/UiAstNode.vue";
import HelloWorld from "@/components/HelloWorld.vue";
import {TestAstNode} from "@/ts/TestAstNode";

export default {
  name: 'App',
  components: {
    HelloWorld,
    UiAstNode
  },
  setup() {
    const codeArea = ref("Here's some code");

    function updateText1() {
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
    return {codeArea, updateText1, graph};
  },
  data() {
    const testGraph = ref(new TestAstNode("A1", "TOP",
        [
          new TestAstNode("AA1", "BOTTOM", [])
        ]
    ));
    return {testGraph: testGraph};
  },
  mounted() {
    this.drawGraph();
  },
  methods: {
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
  <img alt="Vue logo" src="./assets/cobol-rekt-banner.png">
  <HelloWorld header="Welcome to this amazing app"/>
  <UiAstNode :node="testGraph"/>
  <h3>Intermediate representation</h3>
  <div class="readonly-code">What {{ codeArea }}</div>
  <textarea v-model="codeArea" rows="10" columns="10" class="code"/>
  <button @click="updateText1">Le Button</button>
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

.readonly-code {
  font-family: "Courier New", sans-serif;
  background-color: azure;
  border: 1px solid azure;
  text-align: left;
}

.ast_TOP {
  background-color: blue;
}

.ast_BOTTOM {
  background-color: greenyellow;
}

#cyto {
  height: 600px;
  width: 600px;
  background-color: azure;
}
</style>
