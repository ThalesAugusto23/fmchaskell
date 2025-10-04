{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head (x:a) = x
head [] = undefined

tail :: [a] -> [a]
tail (a:xs) = xs
tail [] = undefined

null :: [a] -> Bool
null [] = True
null _ = False

length :: Integral i => [a] -> i
length [] = 0
length (a:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y : snoc x ys

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
(+++) xs []     = xs
(+++) xs [y]    = xs <: y
(+++) xs (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

-- minimum :: Ord a => [a] -> a
minimum :: Ord a => [a] -> a
minimum [] = error "minimum: empty list"
minimum [x] = x
minimum (x:xs) = min x (minimum xs)

-- maximum :: Ord a => [a] -> a
maximum :: Ord a => [a] -> a
maximum [] = error "maximum: empty list"
maximum [x] = x
maximum (x:xs) = max x (maximum xs)

-- take
take :: Int -> [a] -> [a]
take 0 a = []
take a [] = []
take n (x:xs) = x : take (n-1) xs

-- drop
drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop a [] = []
drop n (a:xs) = drop (n-1) xs

-- takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile b (x : xs) = if b x then x : takeWhile b xs else []

-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile b (x : xs) = if b x then dropWhile b xs else x : xs

-- tails
-- init
-- inits

-- subsequences

-- any
-- all

-- and
and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

-- or
or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs

-- concat
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter a [] = []
filter p (x:xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

-- map
map :: (a -> b) -> [a] -> [b]
map a [] = []
map f (x:xs) = f x : map f xs

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

