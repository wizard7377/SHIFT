{-# LANGUAGE CPP #-}

module Extra.Color where

import System.Console.ANSI qualified as ANSI

colorList = ANSI.Red : ANSI.Blue : ANSI.Green : []
getColor :: Int -> ANSI.Color
getColor v = colorList !! mod v 3
colorCode :: ANSI.Color -> String
#ifndef noColor 
colorCode x = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid x]
resetCode = ANSI.setSGRCode []
#else
colorCode x = ""
resetCode = ""
#endif
makeColor color str = colorCode color ++ str ++ resetCode
showColor :: (Show a) => Int -> a -> String
showColor i a = colorCode (getColor i) ++ show a ++ resetCode
showColor' :: Int -> String -> String
showColor' i a = colorCode (getColor i) ++ a ++ resetCode

addColor :: Int -> String -> String
addColor i str = showColor' i str
