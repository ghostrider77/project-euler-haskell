main :: IO()
main = do
    content <- fmap lines (readFile "../resources/P0013.dat")
    let numbers = map read content
    putStrLn $ take 10 $ show $ sum numbers
