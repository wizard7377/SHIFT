# Ops

The operations of ל calculus

`∅ conversion`:
`|- A ~ A`

`α conversion`
`G[L] |- ? ~ דxA ==> ∀y(? ~ A[x := y])`

`ζ reduction`
`G[L] |- ? ~ α[β := γ] ==> ? |> δ` where `δ` is `α` with `β` subsituted for `γ`

`Void reduction`
`(G[L] |- ? ~ (לαβγδ)ε) ==> (G[L] |- ? |> (βε))`

`Left conversion reduction`
`G[L] |- α ~ β, α |> γ ==> G[L] |- γ ~ β`

`Right conversion reduction`
`G[L] |- α ~ β, β |> γ ==> G[L] |- α ~ γ`

`מ reduction`
Goes from `G[L] |- ? ~ (לαβγδ)ε, דαγ ~ ε ==> ? |> דαδ` by application, using fowards reasoning

That is, if for some
`נ conversion`
Given a goal `F` and a term `(לABCD)E`, attempt to unify them through backwards reasoning

`פ reduction`
Apply a level of the fixpoint
`פnA ~ A[ףn := פnA]`

## Examples

For

```
@def T (לx ⊥ x . x) ;
@def Id T ;
@def λ (לx ⊥ x . לA ⊥ A . לx ⊥ x A) ;
@def Λ (λx. λA. לx ⊥ Unit . A) ;
@def N (פ1 (ל_ (לn ⊥ (S n) (ף1 n) Z T ))) ;
@def True (λx.λy.x) ;
@def False (λx.λy.y) ;
@def + פ2
  (ל
    m
    (לn ⊥ Z T)
    (S m) .
    (λn . S (ף2 m n))
  ) ;
```

`(+ (S Z) (S Z))`
`((פ2 (ל
    m 
    (לn ⊥ Z T)
    (S m) .
    (λn . S (ף2 m n))
  )) (S Z) (S Z)` by δ

`(((ל
    m 
    (לn ⊥ Z T)
    (S m) .
    (λn . S ((פ2 (ל
    m 
    (לn ⊥ Z T)
    (S m) .
    (λn . S (ף2 m n))
  )) m n))
  )) (S Z) (S Z))` by fix
