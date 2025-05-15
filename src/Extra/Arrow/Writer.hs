module Extra.Arrow.Writer where

newtype WriterArrowT w a b c = WriterArrow {runWriterArrow :: a b (c, w)}
