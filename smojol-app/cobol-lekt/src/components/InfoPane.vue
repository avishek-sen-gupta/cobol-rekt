<script lang="ts">
import {defineComponent} from 'vue';

export default defineComponent({
  name: "InfoPane",
  props: {
    nodeDetails: {
      type: [Object, null],
      required: true
    }
  }
});

</script>

<template>
  <div id="node-details-pane">
    <div class="pane-heading">Node Data</div>
    <div id="node-details">
      {{ nodeDetails }}
    </div>
  </div>
</template>

<style scoped>

</style>
