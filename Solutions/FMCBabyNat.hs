module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
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

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
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

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus a O = a -- a - 0 = 0
monus O (S _) = O -- 0 - (a > 0) = 0
monus (S a) (S b) = monus a b -- Sa - Sb = a - b

(-*) :: Nat -> Nat -> Nat
(-*) = monus

-- multiplication
(*) :: Nat -> Nat -> Nat
(*) a O = O          -- a * 0 = 0
(*) a (S O) = a      -- a * 1 = a  
(*) a (S b) = a + (a * b)  -- a * (b+1) = a + (a * b)


infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
(^) a O = S O -- a^0 = 1
(^) a (S O) = a -- a^(S O) = a
(^) a (S b) = a * (a ^ b) -- a^b+1 = a * (a^b)

-- decide: infix? ? ^

-- Definindo >=
(>=) :: Nat -> Nat -> Nat
(>=) O O = (S O) -- Retorna true
(>=) O (S a) = O -- Retorna false
(>=) (S a) O = (S O) -- Retorna true
(>=) (S a) (S b) = a b

-- quotient
(/) :: Nat -> Nat -> Nat
(/) a (S O) = a -- a/1 = a
(/) O a = O -- 0/a = 0
(/) a O = undefined
(/) a b =
  case a < b of
    S O -> O 
    O -> S ((a -* b)/b)
   
-- remainder
(%) :: Nat -> Nat -> Nat
(%) a O = undefined
(%) a b = a -* ((a / b) * b)

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
(|||) a O = undefined
(|||) O a = (S O) --Qualquer Nat divide o zero
(|||) a b = isZero (a % b) --Verifica se o resto da divisão é zero


-- Definindo <
(<) :: Nat -> Nat -> Nat
(<) O O = O --Retorna false
(<) O (S a) = S O --Retorna true
(<) (S a) O = O --Retorna false
(<) (S a) (S b) = a < b

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff a O = a  -- |a - 0| = a
absDiff O a = a  -- |0 - b| = b
absDiff a b = 
  case (a <* b) of
    O -> a -* b    -- Quando a >= b: a - b
    (S O) -> b -* a  -- Quando a < b: b - a


(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

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