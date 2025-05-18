module Extra.Boolean where

xor :: Bool -> Bool -> Bool
xor a b = not (a && b) && (a || b)
(%%?) = xor
infix 2 %%?
implication :: Bool -> Bool -> Bool
implication a b = not a || b
(->?) = implication
infixr 4 ->?
