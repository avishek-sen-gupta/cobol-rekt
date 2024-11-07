<script lang="ts">
import {instance} from "@viz-js/viz";
import VueZoomable from "vue-zoomable";
import "vue-zoomable/dist/style.css";

export default {
  name: 'ImageView',
  components: {
    VueZoomable
  },
  props: {
    flowchart: {
      type: String,
      required: true
    }
  },
  mounted() {
    // this.getProjectListing();
  },
  methods: {},
  watch: {
    async flowchart(newValue: string) {
      const viz = await instance();
      const svg = viz.renderSVGElement(newValue, {
        graphAttributes: {
          splines: "ortho"
        }
      });
      document.getElementById("image")!.appendChild(svg);
      // this.$refs.imageWindow.appendChild(svg);
    }
  }
}
</script>

<template>
  <div class="headered-pane" id="image-pane">
    <div class="pane-heading" style="display: flex; align-items: center;">Image</div>
    <div class="zoomable-container">
      <VueZoomable
          style="width: 100%; height: 100%; border: 1px solid black"
          selector="#image"
          :minZoom="0.5"
          :maxZoom="3">
        <div id="image" ref="imageWindow"></div>
      </VueZoomable>
    </div>
  </div>
</template>

<style scoped>

#image-pane {
  height: 500px;
  width: 700px;
}

#image {
  height: 100%;
  width: auto;
  border: 1px solid red;
}

.zoomable-container {
  background-color: azure;
  height: 100%;
  width: 100%;
}
</style>
