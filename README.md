# SHIFT

System for Higher Intuitionistic Flat Types (SHIFT)

Contains:

- Language for Intutionistic Flat Types (LIFT)
- Represenetation for Intutionistic Flat Types (RIFT)
- Solver for Intutionistic Flat Types (SIFT)

## ל Calculus

ל Calculus is a deravitive of λ calculus, with the goal of making it as minimal as possible. While attempts to do the same have gotten to as few as two symbols, they are all similarly flawed: they don't represent simple ideas. Rather, they simply attempt to create a somewhat simple encoding of λ terms.
ל calculus attempts to solve this. It requires only three symbols, and has a simple expressive nature that allows it to be used with ease. In addition, rather than attempting to be an encoding of λ caclulus, it attempts to be a unique system. In addition, as a sidenote, we have had the ability to encode all of mathmatics using only binary since the 1840's, with morse code (which uses only two symbols)

### כלל

There is only one basic constructor in ל calculus, _not_ the ל but rather the כ (Kaf) constructor. The כ constructor is equivalent to a combination of "cons" and application. That is, the way we represent the expression `(3 + 4 = 5)` is firstly to `(= (+ 3 4) 5)` then to `כ(כ=(כ(כ+3)4))5` . We don't actually need the parentheses, as the statement `ככ=ככ+345` is perfectly clear, assuming that all symbols are exactly one grapheme. This then hints at the other part evry symbol that is not Kaf is an atom. That is, our term language consists of only two constructions,

```
כ : ת -> ת -> ת
ט : י -> ת
```

where ת (tav) is the set of terms, and י (Yud) the set of atoms. To this, we need only add one more construction:
Where all variables are one symbol in length

```
מ @ (כככככלVCABI = O)
==> ((V = N, I = A) -> (B = O)) ; (O = כCI)
```

In essence, this means the following, for a given `לVCAB` (in full `ככככלVCAB`): applied to `I`

1. Attempt to unify input (I) and arguement (A), using only variable (V) (with some new (N))
2. If this is succesful, replace the whole application with the body (B) with `V` replaced with `N`
3. If this is impossible, fail without consuming any input with `C` `כCI`
