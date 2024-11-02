<script lang="ts">

import cytoscape from "cytoscape";
import cydagre from "cytoscape-dagre";
import {asCytoscapeModel, ModelNode} from "@/ts/NodeBuilder";
import {defineComponent, PropType, ref} from "vue";

export default defineComponent({
      name: "GraphView",
      props: {
        graphModel: {
          type: [Object, null] as PropType<ModelNode | null>,
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
        graphModel(newValue: ModelNode) {
          this.buildGraph(newValue);
        }
      },
      methods: {
        buildGraph(model: ModelNode) {
          // if (model === undefined) return;
          // if (model instanceof String) return;
          const cytoscapeModel = asCytoscapeModel(model as ModelNode);
          cydagre(cytoscape);
          this.cy = cytoscape({
            container: document.getElementById("cyto"),
            elements: cytoscapeModel,
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
            this.$emit("node-details-changed", {
              id: nodeData.id,
              nodeType: nodeData.nodeType,
            });

            // this.nodeDetails = {
            //   id: nodeData.id,
            //   nodeType: nodeData.nodeType,
            // };
          });
          this.cy.center();
        }
      }
    }
)</script>

<template>
  <div id="graph-view">
    <h3>Graph View</h3>
    <div id="cyto" class="cyto"></div>
  </div>
</template>

<style scoped>

</style>
