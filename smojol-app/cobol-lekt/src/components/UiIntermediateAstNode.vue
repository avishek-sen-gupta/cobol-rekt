<script>

import {defineComponent} from "vue";

export default defineComponent({
      name: "UiIntermediateAstNode",
      props: {
        node: {
          type: Object,
          required: true
        }
      },
      computed: {
        nodeID() {
          return "ast_" + this.node.id;
        },
        isTop() {
          return this.node.nodeType === "TOP";
        },
        isBottom() {
          return this.node.nodeType === "BOTTOM";
        },
        nodeClass() {
          if (this.node.nodeType === "TOP") return "ast_TOP";
          else if (this.node.nodeType === "BOTTOM") return "ast_BOTTOM";
          return "ast_DEFAULT";
        },
        hasChildren() {
          return this.node.childTranspilerNodes.length > 0;
        }
      }
    }
)
</script>

<template>
  <span :id="nodeID" :class="{ast_TOP: isTop, ast_BOTTOM: isBottom}">({{ node.nodeType }}
  <span v-if="hasChildren">
    <UiIntermediateAstNode
        v-for="child in node.childTranspilerNodes"
        :key="child.id"
        :node="child"
    />
  </span>)
  </span>
</template>

<style scoped>
.ast_TOP {
  background-color: lightpink;
}

.ast_BOTTOM {
  background-color: greenyellow;
}
</style>
