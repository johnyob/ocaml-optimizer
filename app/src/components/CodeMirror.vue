<template>
  <div class="vcm-editor-root" ref="editor" />
</template>

<script>
import { EditorState, EditorView, basicSetup } from "@codemirror/basic-setup";

export default {
  name: "CodeMirror",
  props: {
    value: {
      type: String,
      default: "",
    },
  },
  emits: ["update:value"],
  mounted() {
    this.editor = new EditorView({
      state: EditorState.create({
        extensions: [
          basicSetup,
          EditorView.updateListener.of((value) => {
            if (value.docChanged) {
              this.$emit("update:value", this.editor.state.doc.toString());
            }
          }),
        ],
        doc: this.value,
      }),
      parent: this.$refs.editor,
    });
  },
  beforeDestroy() {
    this.editor.destroy();
  },
  watch: {
    value(newValue) {
      const currentValue = this.editor.state.doc.toString();
      if (currentValue === newValue) return;

      const endPosition = currentValue.length;

      this.editor.dispatch({
        changes: {
          from: 0,
          to: endPosition,
          insert: newValue,
        },
      });
    },
  },
};
</script>

<style>
.vcm-editor-root {
  height: 100%;
  width: 100%;
}
.cm-editor {
  height: 100%;
}
</style>
