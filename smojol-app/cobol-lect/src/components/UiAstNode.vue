<script>

import {TestAstNode} from "@/ts/TestAstNode";
import {defineComponent} from "vue";

export default defineComponent({
      name: "UiAstNode",
      props: {
        node: {
          type: TestAstNode,
          required: true
        }
      },
      computed: {
        nodeID() {
          return "ast_" + this.node.id;
        },
        nodeClass() {
          if (this.node.nodeType === "TOP") return "ast_TOP";
          else if (this.node.nodeType === "BOTTOM") return "ast_BOTTOM";
          return "ast_DEFAULT";
        },
        hasChildren() {
          return this.node.children.length > 0;
        }
      }
    }
)
</script>

<template>
  <span :id="nodeID" :class="nodeClass">({{ node.id }}
  <span v-if="hasChildren">
    <UiAstNode
        v-for="child in node.children"
        :key="child.id"
        :node="child"
    />
  </span>)
  </span>
</template>

<style scoped>

</style>
