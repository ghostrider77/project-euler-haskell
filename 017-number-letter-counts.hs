numbers :: [Int]
numbers = map length [
    "", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
    "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]


tens :: [Int]
tens = map length ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]


calcNumberLetterSum :: Int
calcNumberLetterSum =
    let numberSum = sum numbers
        sumFrom1To9 = sum $ take 10 numbers
        sumUpTo99 = foldl (\acc l -> acc + 10 * l + sumFrom1To9) numberSum tens
        hundred = length "hundred"
        thousand = length "thousand"
        oneToNine = take 9 $ drop 1 numbers
        sumTo999 = foldl (\acc l -> acc + l + hundred + 99*(l + hundred + 3) + sumUpTo99) sumUpTo99 oneToNine
    in sumTo999 + numbers !! 1 + thousand


main :: IO()
main = do
    print calcNumberLetterSum
