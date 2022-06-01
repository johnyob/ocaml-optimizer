<template>
  <CodeMirror class="code" v-model:value="code" />
  <div class="graph">
    <DotGraph v-show="dot" v-model:value="dot" />
    <pre class="error" v-if="error">{{ error }}</pre>
    <div class="menu">
      <select v-model="graphKey">
        <option v-for="key in GRAPH_KEYS" :key="key" :value="key">
          {{ key }}
        </option>
      </select>
    </div>
  </div>
</template>

<script>
import CodeMirror from "./components/CodeMirror.vue";
import DotGraph from "./components/DotGraph.vue";
import { get, set } from "idb-keyval";
import { GRAPHS, GRAPH_KEYS } from "./optimizer";

const IDB_KEY = "code";

const defaultProgram = `#f:
  i := 1;
  j := 1;
  k := 0;
#loop:
  if k < 100 then goto #lab1;
  ret;
#lab1:
  if j < 20 then goto #lab2;
  j := k;
  k := k + 2;
  goto #loopend;
#lab2:
  j := i;
  k := k + 1;
#loopend:
  goto #loop;
`;

export default {
  components: { DotGraph, CodeMirror },
  data() {
    return {
      GRAPH_KEYS,
      graphKey: GRAPH_KEYS[0],
      code: "",
      dot: "",
      error: "",
    };
  },
  computed: {
    debounceTimeout() {
      return this.graphKey === "Simple (OPT)" ? 1000 : 250;
    },
  },
  async mounted() {
    const code = (await get(IDB_KEY)) ?? defaultProgram;
    this.code = code;
    this.$watch("code", (newCode) => {
      clearTimeout(this.debounceHandle);
      this.debounceHandle = setTimeout(
        () => this.renderCode(newCode),
        this.debounceTimeout
      );
    });
    await this.renderCode(code);
  },
  methods: {
    renderCode(code) {
      let dot = "";
      let error = "";
      if (code) {
        try {
          const func = globalThis.optimizer[GRAPHS[this.graphKey]];
          dot = func(code);
        } catch (e) {
          error = e[1];
        }
      }
      this.dot = dot;
      this.error = error;
      return set(IDB_KEY, code);
    },
  },
  watch: {
    graphKey() {
      this.renderCode(this.code);
    },
  },
};
</script>

<style>
body {
  margin: 0;
}
#app {
  display: grid;
  grid-template-columns: 1fr 2fr;
  width: 100vw;
  height: 100vh;
  overflow: hidden;
}
.graph {
  position: relative;
}
.menu {
  position: absolute;
  top: 10px;
  right: 10px;
}
.error {
  color: red;
}
</style>
