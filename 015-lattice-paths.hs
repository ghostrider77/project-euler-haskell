import Math.Combinatorics.Exact.Binomial (choose)


main :: IO()
main = do
    let size = 20
    print $ choose (2*size) size
