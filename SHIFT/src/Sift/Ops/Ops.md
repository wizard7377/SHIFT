# Ops

The operations of ל calculus

`∅ conversion`:
`|- A ~ A`

`α conversion`
`דxA ~ ∀y(A[x := y])`

`מ reduction`
Goes from `(לABCD)E` by application, using fowards reasoning

`נ conversion`
Given a goal `F` and a term `(לABCD)E`, attempt to unify them through backwards reasoning

`פ reduction`
Apply a level of the fixpoint
`פnA ~ A[ףn := פnA]`
