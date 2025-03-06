module Extra.Choice where

import Extra.List

-- | The model of choice for syntatic unifification
newtype Choice a = Choice [[a]]
  deriving (Functor, Show)

instance Applicative Choice where
  pure :: a -> Choice a
  pure a = Choice [[a]]
  (<*>) :: Choice (a -> b) -> Choice a -> Choice b
  (Choice func) <*> (Choice val) = Choice $ fmap (\func' -> (fmap (\val' -> (func' <*> val')) val)) func

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
