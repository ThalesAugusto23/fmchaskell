module ExBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

instance Show Bool where

    show True = "True"
    show False = "False"

instance Enum Bool where

    toEnum 0 = False
    toEnum 1 = True

    fromEnum False = 0
    fromEnum True = 1

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
(&&) True True = True
(&&) False False = False
(&&) True False = False
(&&) False True = False

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
(||) True True = True
(||) False False = False
(||) True False = True
(||) False True = True

infixr 2 ||

-- Definindo Not
not :: Bool -> Bool  
not True = False
not False = True

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
(/|\) a b = not (a && b) -- Not And

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
(\|/) a b = not (a || b) -- Not Or

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
(<=/=>) True False = True
(<=/=>) False True = True
(<=/=>) False False = False
(<=/=>) True True = False

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool  
not True = False
not False = True

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True a b = a
ifThenElse False a b = b 

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
(==>) True True = True
(==>) False False = True
(==>) False True = False
(==>) True False = False

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
(<==) a b = b ==> a

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
(<=>) a b = (a ==> b) && (b ==> a)

infixr 1 <=>


