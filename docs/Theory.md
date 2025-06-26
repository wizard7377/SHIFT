# Lamed Calculus

`ל A B C D` = `((A = E, C) -> D) ; B`
`==> (A = E, C, !, D) ; B`
Sample constructions: (`⊥` is fail)
`∀ := (ל x F x ((ל y F ∗ x) . ∗))`, and after internal `מ` reduction, `==מ=> (ל x ⊥ x [y]{x})`
`~ := (ל x T x F)`
`-> := (ל x F x (ל y F y (ל _ F x y))) ≃ (λx.λy.λx.y)`

Reduction rules:

Conversions rules:

`∅` conversion, `A = A`
