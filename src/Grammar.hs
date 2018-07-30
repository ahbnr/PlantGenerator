module Grammar where

data Production a = Production a [a]
type Rules a = [Production a]

applyRules :: Eq a => Rules a -> a -> [a]
applyRules [] x = [x]
applyRules ((Production y result):rs) x
  | y == x     = result
  | otherwise = applyRules rs x

parallelApplication :: Eq a => Rules a -> [a] -> [a]
parallelApplication rules = concat . map (applyRules rules)
