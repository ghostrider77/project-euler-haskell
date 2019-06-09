isPalindrome :: Int -> Bool
isPalindrome n = n == reverseNumber n
    where
        reverseNumber :: Int -> Int
        reverseNumber = read . reverse . show


largestPalindromeProduct :: Int
largestPalindromeProduct = maximum [ a * b | a <- [100..999], b <- [a..999], isPalindrome (a * b) ]


main :: IO()
main = do
    let result = largestPalindromeProduct
    print result
