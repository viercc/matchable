module Main where

import MiniProlog

main :: IO ()
main = do
  -- Solving simple substitution
  query ["X"] $ var "X" === cons (var "Y") (var "Z") .&.
                var "Y" === lit 1 .&.
                var "Z" === nil
  -- Solving X ++ X == [1,1]
  let makeList = foldr (cons . lit) nil
  query ["X"] $ append (var "X") (var "X") (makeList [1,1])
  -- Solving X ++ X == [1,1,1], which has no solution.
  query ["X"] $ append (var "X") (var "X") (makeList [1,1,1])
  -- Solving X ++ X ++ X == [1,2,1,2,1,2]
  query ["X"] $ append (var "X") (var "X") (var "Y") .&.
                append (var "X") (var "Y") (makeList [1,2,1,2,1,2])
