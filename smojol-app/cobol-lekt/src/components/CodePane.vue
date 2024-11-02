<script lang="ts">

import {defineComponent, PropType} from "vue";
import {TreeModelNode} from "@/ts/CytoTree";
import UiIntermediateAstNode from "@/components/UiIntermediateAstNode.vue";

export default defineComponent({
  components: {
    UiIntermediateAstNode
  },
  props: {
    irAST: {
      type: [Object, null] as PropType<TreeModelNode | null>,
      required: true
    }
  },
  computed: {
    irTreePopulated() {
      return this.irAST != null;
    }
  }
})
</script>

<template>
  <div class="headered-pane" id="code-view">
    <div class="pane-heading">Intermediate Form Source</div>
    <div class="readonly-code code-pane">
      <UiIntermediateAstNode :node="irAST" :depth="0" v-if="irTreePopulated"/>
    </div>
  </div>
</template>

<style scoped>
</style>
