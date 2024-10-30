<script>

import {TestAstNode} from "@/ts/TestAstNode";

export default ({
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
        }
      }
    }
)
</script>

<template>
  <span :id="nodeID" :class="nodeClass">({{ node.id }}</span>
  <span>ast-{{ node.nodeType }}</span>
  <span>
    <UiAstNode
        v-for="child in node.children"
        :key="child.id"
        :node="child"
    />
  </span>)
</template>

<style scoped>

</style>
