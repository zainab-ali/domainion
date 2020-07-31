{-# LANGUAGE FlexibleInstances #-}
-- We want to make an instance for String, but String is a type synonym
module Final where

import Prelude (Int, String, id, show, (<>))
import qualified Prelude as P ((+), (*))

class IntSYM repr where
  lit :: Int -> repr
  (+) :: repr -> repr -> repr

expr :: IntSYM repr => repr
expr = (lit 1 + lit 2) + lit 3

instance IntSYM Int where
  lit = id
  (+) = (P.+)

eval :: Int -> Int
eval = id

instance IntSYM String where
  lit = show
  x + y = "(" <> x <> " + " <> y <> ")"

pretty :: String -> String
pretty = id

-- Prelude Final> pretty expr1
-- "((1 + 2) + 3)"

class MulSYM repr where
  (*) :: repr -> repr -> repr

instance MulSYM Int where
  x * y = x P.* y

instance MulSYM String where
  x * y = "(" <> x <> " * " <> y <> ")"

-- Redeclare some things from initial to play with
program :: Int
program = 1 + 2 + 3

text :: String
text = "((1 + 2) + 3)"

expr2 :: (IntSYM repr, MulSYM repr) => repr
expr2 = (lit 1 * lit 2) + (lit 3)
-- Prelude Final> pretty expr2
-- "((1 * 2) + 3)"
