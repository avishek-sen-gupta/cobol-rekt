<script lang="ts">

import cytoscape from "cytoscape";
import cydagre from "cytoscape-dagre";
import {asCytoscapeTree, TreeModelNode, TranspilerNodeChildrenAccess} from "@/ts/CytoTree";
import {defineComponent, PropType, ref} from "vue";
import {asCytoscapeDigraph, Digraph} from "@/ts/Digraph";
import {CytoModel} from "@/ts/CytoscapeTypes";
import {MutableCenter} from "@/ts/FlippableId";
import {LoopBody, LoopNode} from "@/ts/ContractTypes";

export default defineComponent({
      name: "GraphView",
      props: {
        treeModel: {
          type: [Object, null] as PropType<TreeModelNode | null>,
          required: true
        },
        loopBodies: {
          type: Object as PropType<LoopNode[]>,
          required: true
        },
        digraphModel: {
          type: [Object, null] as PropType<Digraph | null>,
          required: true
        },
        centerNode: {
          type: [Object, null] as PropType<MutableCenter | null>,
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
        },
        centerNode(newValue: MutableCenter, oldValue: MutableCenter | null) {
          console.log(oldValue);
          if (this.cy === null) return;
          const elementById = this.cy.getElementById(newValue.id);
          if (oldValue !== null) {
            const oldElementById = this.cy.getElementById(oldValue.id);
            oldElementById.unselect();
          }

          this.cy.center(elementById);
          elementById.select();
        },
        loopBodies(newValue: LoopBody[]) {
          const loopNodes = newValue.flatMap(body => body.loopNodes);
          const allLoopNodeIDs = loopNodes.map(ln => this.cy.getElementById(ln.id));
          allLoopNodeIDs.forEach(ele => ele.style("background-color", "green"))
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
                  'background-color': "purple",
                  'label': 'data(id)',
                  "transition-property": "background-color",
                  'transition-duration': 500,
                  "transition-timing-function": "ease-in-out",
                }
              },
              {
                selector: 'node:selected',
                style: {
                  "border-color": "red",
                  "border-width": "2px",
                  'background-color': 'yellow',
                  'label': 'data(id)',
                  "transition-property": "background-color",
                  'transition-duration': 500,
                  "transition-timing-function": "ease-in-out",
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
}

.flashingnode {
  border: 3px solid red;
  background-color: yellow;
  transition: background-color 3s ease;}
</style>
