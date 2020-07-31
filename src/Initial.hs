module Initial where

-- A simple introduction to DSLs

-- Let's say we want this program
program :: Int
program = 1 + 2 + 3

-- And also this text
text :: String
text = "((1 + 2) + 3)"

-- We could use a single representation
data Exp = Lit Int | Add Exp Exp

expr = Add (Add (Lit 1) (Lit 2)) (Lit 3)

eval :: Exp -> Int
eval (Lit i) = i
eval (Add x y) = eval x + eval y

pretty :: Exp -> String
pretty (Lit i) = show i
pretty (Add x y) = "(" <> pretty x <> " + " <> pretty y <> ")"

text1 :: String
text1 = pretty expr
-- "((1 + 2) + 3)"

program1 :: Int
program1 = eval expr
-- 6


data BadMulExp = Mul BadMulExp BadMulExp | Wrap Exp

-- expr1 = Add (Mul (Wrap (Lit 1)) (Wrap (Lit 2))) (Lit 3)
-- Quite rightly:
--    • Couldn't match expected type ‘Exp’ with actual type ‘BadMulExp’
--    • In the first argument of ‘Add’, namely
--        ‘(Mul (Wrap (Lit 1)) (Wrap (Lit 2)))’

data MulExp = Lit' Int | Add' MulExp MulExp | Mul' MulExp MulExp

expr2 = Add' (Mul' (Lit' 1) (Lit' 2)) (Lit' 3)

-- We also need to redefine pretty and eval

-- Proponents of the initial encoding will rightly point out that you can get around that with fixed-points
