import Data.Strings (strReplace)


mapping :: [(String, String)]
mapping = [("DDDD", "MM"), ("DCCCC", "CM"), ("CCCC", "CD"), ("LXXXX", "XC"), ("LLLL", "CC"), ("XXXX", "XL"),
           ("VVVV", "XX"), ("VIIII", "IX"), ("IIII", "IV")]


shortenRomanNumerals :: [String] -> Int
shortenRomanNumerals numerals = foldl processNumber 0 numerals
    where
        processNumber :: Int -> String -> Int
        processNumber acc number =
            let nrCharacters = length number
                compactForm = foldl (\acc (orig, replacement) -> strReplace orig replacement acc) number mapping
            in acc + nrCharacters - (length compactForm)


main :: IO()
main = do
    content <- fmap lines (readFile "../resources/P0089.dat")
    print $ shortenRomanNumerals content
