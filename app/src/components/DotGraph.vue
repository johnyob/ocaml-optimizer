<template>
  <div class="dot-graph-root">
    <div class="dot-graph" v-html="graph" :style="transformStyles" />
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
    return { graph: "", translateX: 0, translateY: 0, scale: 1 };
  },
  mounted() {
    this.$el.addEventListener("mousemove", this.onMouseMove);
    this.$el.addEventListener("wheel", this.onWheel);
    window.addEventListener("keydown", this.onKeyDown);
  },
  beforeDestroy() {
    this.$el.removeEventListener("mousemove", this.onMouseMove);
    this.$el.removeEventListener("wheel", this.onWheel);
    window.removeEventListener("keydown", this.onKeyDown);
  },
  computed: {
    transformStyles() {
      const transform = `translate(${this.translateX}px, ${this.translateY}px) scale(${this.scale})`;
      return { transform };
    },
  },
  methods: {
    onMouseMove(e) {
      if (e.buttons !== 1) return;
      this.translateX += e.movementX;
      this.translateY += e.movementY;
    },
    onWheel(e) {
      this.scale = Math.max(1, Math.min(20, this.scale - e.deltaY * 0.01));
    },
    onKeyDown(e) {
      if (e.ctrlKey && e.key === "r") {
        this.translateX = 0;
        this.translateY = 0;
        this.scale = 1;
      }
    },
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
  overflow: hidden;
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
