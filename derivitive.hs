data Expression = Number Int |
                  Variable String |
                  Sum Expression Expression |
                  Subtract Expression Expression |
                  Power Expression Expression |
                  Product Expression Expression
  deriving Eq

instance Show Expression where
  show (Number i) = show i
  show (Variable s) = s
  show (Sum l r) = "(" ++ show l ++ " + " ++ show r ++ ")"
  show (Subtract l r) = "(" ++ show l ++ " - " ++ show r ++ ")"
  show (Power b e) = "(" ++ show b ++ " ^ " ++ show e ++ ")"
  show (Product l r) = "(" ++ show l ++ " * " ++ show r ++ ")"

derive :: Expression -> Expression
derive (Number _) = Number 0
derive (Variable _) = Number 1
-- Sum rule
derive (Sum l r) = Sum (derive l) (derive r)
-- Subtraction rule
derive (Subtract l r) = Subtract (derive l) (derive r)
-- Constant division rule
derive (Product (Number n) e) = (Product (Number n) (derive e))
-- Product rule (Leibniz rule)
derive (Product l r) = (Sum (Product (derive l) r) (Product l (derive r)))
-- Power rule
derive (Power b (Number n)) = Product (Number n) (Power b (Number (n - 1)))
derive (Power _ _) = undefined

-- This isn't exhaustive. It just covers cases I saw while testing.
simplify' :: Expression -> Expression
simplify' (Sum (Number l) (Number r)) = Number (l + r)
simplify' (Sum (Number l) r) = (Sum r (Number l))
simplify' (Sum e (Number 0)) = simplify' e
simplify' (Sum (Sum ll (Number lr)) (Number r)) = (Sum (Number (r + lr)) ll)
simplify' (Sum l r) = Sum (simplify' l) (simplify' r)

simplify' (Subtract (Number l) (Number r)) = Number (l - r)
simplify' (Subtract (Number l) r) = (Subtract r (Number l))
simplify' (Subtract e (Number 0)) = simplify' e
simplify' (Subtract (Subtract ll (Number lr)) (Number r)) = (Subtract (Number (r - lr)) ll)
simplify' (Subtract l r) = Subtract (simplify' l) (simplify' r)

simplify' (Product (Number l) (Number r)) = Number (l * r)
simplify' (Product l (Number r)) = (Product (Number r) l)
simplify' (Product (Number 0) _) = Number 0
simplify' (Product (Number 1) e) = simplify' e
simplify' (Product (Number l) (Product (Number rl) rr)) = Product (Number (l * rl)) rr
simplify' (Product l r) = Product (simplify' l) (simplify' r)

simplify' (Power (Number l) (Number r)) = Number (l ^ r)
simplify' (Power _ (Number 0)) = Number 0
simplify' (Power e (Number 1)) = simplify' e

simplify' e = e

-- http://stackoverflow.com/questions/7442892/repeatedly-applying-a-function-until-the-result-is-stable
converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys
converge _ _ = undefined

simplify :: Expression -> Expression
simplify = converge (==) . iterate simplify'

-- 9x^2 + 8x + 7
expression :: Expression
-- expression = (Sum (Sum (Power (Product (Number 9) (Variable "x")) (Number 2)) (Product (Number 8) (Variable "x"))) (Number 7))
expression = (Sum (Sum (Number 8) (Variable "x")) (Number 7))

main :: IO()
main = print $ simplify $ expression

