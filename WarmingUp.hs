module WarmingUp where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled xs = ((,) <$> cap <*> rev) xs

monaded :: [Char] -> ([Char], [Char])
monaded = do
  a <- cap
  b <- rev
  return (a, b)

binded :: [Char] -> ([Char], [Char])
binded xs = (cap <$> rev) >>= (,) $ xs
