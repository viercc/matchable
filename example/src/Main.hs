module Main where

import MiniProlog

main :: IO ()
main = do
  putStrLn "Solving simple substitution"
  query ["X","Y","Z"] $
    var "X" === var "Y" .: var "Z" >>
    var "Y" === lit 4 >>
    var "Z" === nil
  
  let makeList :: [Int] -> Term
      makeList = foldr ((.:) . lit) nil
  putStrLn "Solving X ++ X == [1,1]"
  query ["X"] $ append (var "X") (var "X") (makeList [1,1])
  
  putStrLn "Solving X ++ X == [1,1,1], which has no solution."
  query ["X"] $ append (var "X") (var "X") (makeList [1,1,1])
  
  putStrLn "Solving X ++ X ++ X == [1,2,1,2,1,2]"
  query ["X"] $ append (var "X") (var "X") (var "Y") >>
                append (var "X") (var "Y") (makeList [1,2,1,2,1,2])
