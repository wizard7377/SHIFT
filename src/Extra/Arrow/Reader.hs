module Extra.Arrow.Reader where

newtype ReaderArrowT r a b c = ReaderArrow {runReaderArrow :: a (b, r) c}
