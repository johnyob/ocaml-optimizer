import "./lib.ocaml";

const GRAPHS = {
  Simple: "simpleFromString",
  "Simple (AVAIL)": "simpleAvailFromString",
  "Simple (LIVE)": "simpleLiveFromString",
  "Simple (OPT)": "simpleOptimizedFromString",
  "Simple (REACH)": "simpleReachFromString",
  Basic: "basicBlockFromString",
  SSA: "ssaFromString",
};
const GRAPH_KEYS = Object.keys(GRAPHS);

export { GRAPHS, GRAPH_KEYS };
