{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# LANGUAGE FlexibleContexts #-}
module InvertedIndex
    (
    invertedIndex
    ) where
import Data.Tuple.Select ( Sel1(..), Sel2(..) )



invertedIndex :: (Foldable t, Eq a1, Sel1 a2 String, Sel2 a2 [a1]) =>a1 -> t a2 -> [(a1, String)]
invertedIndex word mappedWords = do
    map(\s3->(word, s3)) $ concatMap(\s -> filter(/= " ") $ map(\s2 -> if word == s2 then sel1 s else " ") $ sel2 s) mappedWords
