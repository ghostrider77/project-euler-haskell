import Control.Monad (guard)


solveDigitCancellingFractionsProblem :: Int
solveDigitCancellingFractionsProblem = denom `div` gcd'
    where
        digits = [1..9]
        curiousFractions = do
            a <- digits
            b <- digits
            c <- digits
            guard (b /= c && c*(10*a + b) == (10*b + c)*a && 10*a + b < 10*b + c)
            return (10*a + b, 10*b + c)
        (num, denom) = foldl (\(a, b) (x, y) -> (a*x, b*y)) (1, 1) curiousFractions
        gcd' = gcd num denom


main :: IO()
main = do
    print solveDigitCancellingFractionsProblem
