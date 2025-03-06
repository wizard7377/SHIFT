module Extra.Choice where

newtype Choice a = Choice [[a]]

simple :: a -> Choice a
simple a = Choice [[a]]
trivial = Choice [[]]
cabsurd = Choice []
choiceAnd :: Choice a -> Choice a -> Choice a
choiceAnd (Choice as) (Choice bs) = Choice $ (++) <$> as <*> bs
choiceOr :: Choice a -> Choice a -> Choice a
choiceOr (Choice as) (Choice bs) = Choice $ as ++ bs

(<&&>) = choiceAnd
(<||>) = choiceOr
infixl 9 <&&>
infixl 8 <||>

solve :: ([a] -> Bool) -> Choice a -> Choice a
solve prop (Choice choice) = Choice $ filter prop choice
resolve :: Choice a -> [[a]]
resolve (Choice choice) = choice
