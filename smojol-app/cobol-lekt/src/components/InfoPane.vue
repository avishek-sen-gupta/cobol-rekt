<script lang="ts">
import {defineComponent} from 'vue';
import JsonTree from "@/components/JsonTree.vue";

export default defineComponent({
  name: "InfoPane",
  components: {
    JsonTree
  },
  props: {
    nodeDetails: {
      type: [Object, null],
      required: true
    }
  },
  computed: {
    nodeDetailsExist() {
      return this.nodeDetails !== null;
    }
  }
});

</script>

<template>
  <div class="headered-pane" id="node-info-pane">
    <div class="pane-heading">Node Data</div>
    <div id="node-details" class="clt" style="overflow-x: auto">
      <JsonTree entry-key="__ROOT__" v-if="nodeDetailsExist" :entry-value="nodeDetails"/>
    </div>
  </div>
</template>

<style scoped>
#node-details {
  height: 100%;
  font-family: Monaco, sans-serif;
}
</style>
