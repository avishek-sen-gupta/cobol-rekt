<script lang="ts">

import {defineComponent} from "vue";
import {isPrimitive} from "@/ts/TypeUtils";

export default defineComponent({
      name: "JsonTree",
      props: {
        entryValue: {
          type: null,
          required: true
        },
        entryKey: {
          type: String,
          required: true
        }
      },
      data() {
        return {expanded: true};
      },
      methods: {
        isPrimitive,
        toggleExpand() {
          this.expanded = !this.expanded;
        }
      },
      computed: {
        isRoot() {
          return this.entryKey === "__ROOT__";
        }
      }
    }
)
</script>

<template>
  <li v-if="isPrimitive(entryValue)">
    {{ entryKey }}: <strong>{{ entryValue }}</strong>
  </li>
  <div v-else-if="isRoot">
    <span @click="toggleExpand">ROOT</span>
    <ul v-if="expanded">
      <JsonTree
          v-for="key in Object.keys(entryValue)"
          :key="key"
          :entryKey="key"
          :entryValue="entryValue[key]"
      />
    </ul>
  </div>
  <li v-else>
    <button v-if="!expanded" @click="toggleExpand" class="clickable-item">&#8862;</button>
    <button v-if="expanded" @click="toggleExpand" class="clickable-item">&#8863;</button>
    <span>{{ entryKey }}</span>
    <ul v-if="expanded">
      <JsonTree
          v-for="key in Object.keys(entryValue)"
          :key="key"
          :entryKey="key"
          :entryValue="entryValue[key]"
      />
    </ul>
  </li>
</template>
<style scoped>
</style>
