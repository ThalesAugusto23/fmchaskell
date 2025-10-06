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
product [] = (S O)
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x] -- Concatena o resto com o início

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y : snoc x ys -- Adiciona o elemento no final (literalmente o contrário do cons)

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
minimum [] = undefined
minimum [x] = x -- Só tem um elemento, então ele é o min
minimum (x:xs) = min x (minimum xs) -- Compare o X com o min de xs

-- maximum :: Ord a => [a] -> a
maximum :: Ord a => [a] -> a
maximum [] = undefined
maximum [x] = x -- Só tem um elemento, então ele é o max
maximum (x:xs) = max x (maximum xs) -- Compare o X com o max de xs

-- take
take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs -- Pego elementos da lista

-- drop
drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs -- Removo elementos da lista

-- takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile b (x : xs) = if b x then x : takeWhile b xs else [] -- While true, take 

-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile b (x : xs) = if b x then dropWhile b xs else x : xs -- While true, ignore

-- tails

-- init
init :: [a] -> [a]
init [_] = [] -- Tem um elemento, retorne vazio
init (x:xs) = x : init xs  -- Mantém o primeiro elemento, retira o último do resto

-- inits

-- subsequences

-- any                 
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False -- Lista vazia, nenhum elemento satisfaz a condição (alguém satisfaz?)
any p (x:xs) = p x || any p xs -- Testa o primeiro elemento e testa o resto da lista (ou um ou outro satisfaz?)

-- all  
all :: (a -> Bool) -> [a] -> Bool 
all _ [] = True -- Todos satisfazem! Lista vazia = True
all p (x:xs) = p x && all p xs -- Testa o primeiro e se todos os outros satisfazem

-- and
and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs -- Testa se ambos satisfazem

-- or
or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs -- Testa se um ou outro satisfaz

-- concat
concat :: [[a]] -> [a]
concat [] = [] -- Não há elementos para concatenar
concat (x:xs) = x ++ concat xs  

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = [] -- Não há elementos para filtrar (lista vazia)
filter f (x : xs) = if f x then (x : filter f xs) else filter f xs

-- map
map :: (a -> b) -> [a] -> [b]
map _ [] = [] -- Não há elementos para transformar (lista vazia)
map f (x:xs) = f x : map f xs

-- cycle

-- repeat
repeat :: a -> [a]
repeat x = x : repeat x -- Lista onde o elemento se repete infinitas vezes

-- replicate
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x -- Lista onde o elemento se repete 'n' vezes

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
zip :: [a] -> [b] -> [(a,b)]
zip [] _ = [] 
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys -- Combinação de duas listas (nenhuma vazia)

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

