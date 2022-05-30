import Viz from "viz.js";
import workerURL from "viz.js/full.render.js?url";

let viz = new Viz({ workerURL } as any);

export async function renderDot(dot: string): Promise<string> {
  try {
    return await viz.renderString(dot, { format: "svg" });
  } catch (e) {
    viz = new Viz({ workerURL } as any);
    throw e;
  }
}
