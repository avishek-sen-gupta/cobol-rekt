<script lang="ts">

import cytoscape from "cytoscape";
import cydagre from "cytoscape-dagre";
import {asCytoscapeTree, TreeModelNode, TranspilerNodeChildrenAccess} from "@/ts/CytoTree";
import {defineComponent, PropType, ref} from "vue";
import {asCytoscapeDigraph, Digraph} from "@/ts/Digraph";
import {CytoModel} from "@/ts/CytoscapeTypes";

export default defineComponent({
      name: "GraphView",
      props: {
        treeModel: {
          type: [Object, null] as PropType<TreeModelNode | null>,
          required: true
        },
        digraphModel: {
          type: [Object, null] as PropType<Digraph | null>,
          required: true
        }
      },
      setup() {
        const cy = ref<cytoscape.Core | null>(null);
        return {cy};
      },
      mounted() {
        // this.buildGraph();
      },
      watch: {
        treeModel(newValue: TreeModelNode) {
          this.buildGraph(asCytoscapeTree(newValue, TranspilerNodeChildrenAccess));
        },
        digraphModel(newValue: Digraph) {
          this.buildGraph(asCytoscapeDigraph(newValue));
        }
      },
      methods: {
        buildGraph(cytoscapeModel: CytoModel) {
          cydagre(cytoscape);
          this.cy = cytoscape({
            container: document.getElementById("cyto"),
            elements: cytoscapeModel,
            style: [
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
            this.$emit("node-details-changed", {
              id: nodeData.id,
              nodeType: nodeData.nodeType,
            });
          });
          this.cy.center();
        }
      }
    }
)</script>

<template>
  <div class="headered-pane" id="graph-view">
    <div class="pane-heading">Graph View</div>
    <div id="cyto" class="cyto"></div>
  </div>
</template>

<style scoped>
#cyto {
  height: 100%;
  background-color: azure;
  border: 1px solid;
}
</style>
