# OCaml Optimizer ⏱️

This tool implements various analyses and optimizations on a simple
C-like language (based on 3-address codes) in OCaml. The implementation of these analyses 
and optimizations closely follows the Cambridge [Optimizing Compilers](https://www.cl.cam.ac.uk/teaching/2122/OptComp/) course. 


## Features

- Dataflow analysis framework 📊
- Flowgraph construction (including basic blocks) 💧
- SSA transformation ✨
- Many optimizations: 
  - Peephole optimizations 👀
  - Unreachable code elimination ⛔️
  - Dead code elimination 💀
  - Common subexpression elimination  🧲
  - Copy propagation  📋
  - Constant propagation  🌱
- Dominance tree calculator 🌲
- A [godbolt](https://godbolt.org/)-like web explorer 🌐


## Install

This tool is written in OCaml and Typescript. You'll need to install OCaml version 4.12.0, `npm` and `opam`.
Once installed, run these commands to get started!
```sh
$ make install
$ make build
```

## Using the Web Explorer

To start the web explorer frontend, run
```sh
$ make run
```

## Syntax

The C-like language (based on [3-address codes](https://www.cl.cam.ac.uk/teaching/2122/OptComp/extra/3addrcode.txt)) is given by the following grammar:

```ocaml
(* Expressions *) 
  e ::= x                       (* Variables *)
      | n                       (* Integers *)
      | e + e                   (* Addition *)
      | e - e                   (* Subtraction *)
      | e * e                   (* Multiplication *)
    
(* Conditions *)
  b ::= e = e                   (* Equality *)
      | e <> e                  (* Not Equal *)
      | e > e                   (* Greater Than *)
      | e < e                   (* Less Than *)

(* Instructions/Commands *) 
  i ::= x := e                  (* Assignment *) 
      | if b then goto label    (* Conditional Branching *)
      | label(e, ..., e)        (* Procedure Call *)
      | [x](e, ..., e)          (* Indirect Procedure Call *)
      | ret                     (* Procedure Return *)
      | goto label              (* Goto *)
      | goto [x]                (* Indirect Goto *)
      | x := &(x|label)         (* Address of [x] or [label]*)
      | x := [e]                (* Pointer Dereference *)
      | [x] := e                (* Pointer Assignment *)
```

Programs are simple linear sequences of instructions (semi-colon separated). 

## Authors
- [Alistair O'Brien](https://github.com/johnyob)
- [Brendan Coll](https://github.com/mrbbot)
