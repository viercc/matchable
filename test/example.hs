{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Main(main) where

import           Data.Matchable
import qualified Data.Map       as Map

import GHC.Generics(Generic1)
import Data.Functor.Classes

main :: IO ()
main = do
  p $ "> zipMatch " ++ show list1 ++ " " ++ show list2
  p $ show (zipMatch list1 list2)
  p $ "> zipMatch " ++ show list1 ++ " " ++ show list3
  p $ show (zipMatch list1 list3)
  p $ "> zipMatch " ++ show list1 ++ " (repeat 0)"
  p $ show (zipMatch list1 list4)
  p "-------------------------------------------------"
  p $ "> zipMatch " ++ show pair1 ++ " " ++ show pair2
  p $ show (zipMatch pair1 pair2)
  p $ "> zipMatch " ++ show pair3 ++ " " ++ show pair4
  p $ show (zipMatch pair3 pair4)
  p $ "> zipMatch " ++ show pair1 ++ " " ++ show pair3
  p $ show (zipMatch pair1 pair3)
  p "-------------------------------------------------"
  p $ "> zipMatch (" ++ show either1 ++ ") (" ++ show either2 ++ ")"
  p $ show (zipMatch either1 either2)
  p $ "> zipMatch (" ++ show either1 ++ ") (" ++ show either3 ++ ")"
  p $ show (zipMatch either1 either3)
  p $ "> zipMatch (" ++ show either3 ++ ") (" ++ show either3 ++ ")"
  p $ show (zipMatch either3 either3)
  p $ "> zipMatch (" ++ show either3 ++ ") (" ++ show either4 ++ ")"
  p $ show (zipMatch either3 either4)
  p "-------------------------------------------------"
  p $ "> zipMatch (" ++ show map1 ++ ") (" ++ show map2 ++ ")"
  p $ show (zipMatch map1 map2)
  p $ "> zipMatch (" ++ show map1 ++ ") (" ++ show map3 ++ ")"
  p $ show (zipMatch map1 map3)
  p "-------------------------------------------------"
  p $ "> zipMatch (" ++ show myTree1 ++ ") (" ++ show myTree2 ++ ")"
  p $ show (zipMatch myTree1 myTree2)
  p $ "> zipMatch (" ++ show myTree1 ++ ") (" ++ show myTree3 ++ ")"
  p $ show (zipMatch myTree1 myTree3)
  p $ "> zipMatch (" ++ show myTree1 ++ ") (" ++ show myTree4 ++ ")"
  p $ show (zipMatch myTree1 myTree4)
  where p = putStrLn

list1, list2, list3, list4 :: [Int]
list1 = [0,1,2]
list2 = [3,4,5]
list3 = [0,1]
list4 = repeat 0

either1, either2, either3, either4 :: Either String Int
either1 = Right 1
either2 = Right 2
either3 = Left "foo"
either4 = Left "bar"

pair1, pair2, pair3, pair4 :: (Char, Int)
pair1 = ('a', 1)
pair2 = ('a', 2)
pair3 = ('b', 3)
pair4 = ('b', 4)

map1, map2, map3 :: Map.Map Char Int
map1 = Map.fromList [pair1, pair3]
map2 = Map.fromList [pair2, pair4]
map3 = Map.fromList [pair1]

data MyTree k a = Empty | Node k a (MyTree k a) (MyTree k a)
  deriving (Show, Eq, Functor, Generic1)

instance Eq k => Eq1 (MyTree k) where
  liftEq = liftEqDefault

instance Eq k => Matchable (MyTree k) where
  zipMatchWith = genericZipMatchWith

myTree1 :: MyTree Int String
myTree1 = Node 0 "foo" Empty (Node 1 "bar" Empty Empty)

myTree2 :: MyTree Int Int
myTree2 = Node 0 200 Empty (Node 1 300 Empty Empty)

myTree3 :: MyTree Int Bool
myTree3 = Node 3 True Empty (Node 4 False Empty Empty)

myTree4 :: MyTree Int Char
myTree4 = Node 0 'a' Empty Empty
