<script lang="ts">

import {defineComponent, PropType} from "vue";
import {TreeModelNode} from "@/ts/CytoTree";
import UiIntermediateAstNode from "@/components/UiIntermediateAstNode.vue";

export default defineComponent({
  components: {},
  methods: {},
  props: {
    source: {
      type: [Object, null] as PropType<any | null>,
      required: true
    }
  }
})
</script>

<template>
  <div class="headered-pane" id="code-view">
    <div class="pane-heading">Raw Source</div>
    <div class="readonly-code code-pane borland-theme" v-if="source !== null">
      <span>{{ source.ast.text }}</span>
    </div>
    <div class="readonly-code code-pane scope-theme" v-else>
    </div>
  </div>
</template>

<style scoped>
</style>
