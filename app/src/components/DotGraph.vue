<template>
  <div class="dot-graph-root">
    <div class="dot-graph" v-html="graph" />
  </div>
</template>

<script>
import { renderDot } from "../viz";

export default {
  name: "DotGraph",
  props: {
    value: {
      type: String,
      default: "",
    },
  },
  data() {
    return { graph: "" };
  },
  watch: {
    value: {
      immediate: true,
      async handler(newValue) {
        this.graph = newValue ? await renderDot(newValue) : "";
      },
    },
  },
};
</script>

<style>
.dot-graph-root {
  width: 100%;
  height: 100%;
  position: relative;
}
.dot-graph {
  position: absolute;
  top: 0;
  left: 0;
  bottom: 0;
  right: 0;
  display: flex;
  align-items: center;
  justify-content: center;
}
.dot-graph > svg {
  max-width: 100%;
  max-height: 100%;
}
</style>
