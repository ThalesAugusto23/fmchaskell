{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show = undefined

instance Eq Nat where

    (==) = undefined

instance Ord Nat where

    (<=) = undefined

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min = undefined

    max = undefined


----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Nat
isZero O = S O -- Se digitar zero, a função vai retornar verdadeiro (retorna 1)
isZero (S n) = O -- Se ditigar algo != 0, a função vai retornar falso (retorna 0)

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O -- pred de zero é zero
pred (S n) = n -- pred de Sn é n

-- Output: O means False, S O means True
even :: Nat -> Nat
even O = S O  -- zero é par? sim, retorna S O (true)
even (S O) = O -- um é par? não, retorna O (false)
even (S(S n)) = even n -- o succ do succ de N é par? retorna se N é par

odd :: Nat -> Nat
odd O = O -- zero é ímpar? não, retorna O (false)
odd (S O) = S O -- um é ímpar? sim, retorna S O (true)
odd (S(S n)) = odd n -- o succ do succ de N é ímpar? retorna se N é ímpar

----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) = undefined

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.

monus :: Nat -> Nat -> Nat
monus a O = a -- a - 0 = 0
monus O (S _) = O -- 0 - (a > 0) = 0
monus (S a) (S b) = monus a b -- Sa - Sb = a - b

(-*) :: Nat -> Nat -> Nat
(-*) = undefined

-- multiplication
times :: Nat -> Nat -> Nat
times = undefined

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow = undefined

exp :: Nat -> Nat -> Nat
exp = undefined

(<^>) :: Nat -> Nat -> Nat
(<^>) = undefined

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) = undefined

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) = undefined

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv = undefined

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) = undefined

divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist = undefined

(|-|) = dist

factorial :: Nat -> Nat
factorial O = (S O) -- Fatorial de 0 é 1 
factorial (S O) = (S O) -- Fatorial de 1 é 1
factorial (S a) = (S a) * factorial a -- Fatorial de a é a * (a-1)!

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O  -- Zero retorna O
sg (S a) = S O -- Qualquer número positivo retorna (S O)

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo a O = undefined  -- log a(0) 
lo O a = undefined  -- log 0(a)  
lo (S O) a = undefined -- log 1(a) 
lo a (S O) = O -- log a(1) = 0 
lo b a = S (lo b (a / b)) -- succ de log b(a/b)


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat = undefined

fromNat :: Integral a => Nat -> a
fromNat = undefined


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = undefined
      | otherwise = undefined

