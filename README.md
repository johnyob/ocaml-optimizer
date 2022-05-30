# ocaml-optimizer
Implementation of various analyses and optimizations on 3-address codes

Semantics:
```
x := e
if cond then goto label
lab(e1, .., en)
[x](e1, ..., en)
goto label
goto [x]
ret

(* gets address of x or lab and stores in x *)
x := &(x|lab)

(* load value pointed to by e into x *)
x := [e]
def = { x }
ref = all address taken variables \cup ref(e)

(* computes e, stores in location pointed to by x *)
[x] := e
def = emptyset
ref = ref(e) \cup { x }
```
