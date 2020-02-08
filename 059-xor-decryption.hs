import Data.List.Split (splitOn)
import Data.Char (chr, ord)
import Data.Bits (xor)


mostCommonEnglishWords :: [String]
mostCommonEnglishWords = ["the", "be", "to", "of", "and", "a", "in", "that", "have", "i"]


asciiCodeSumOfDecryptedText :: [Int] -> Int
asciiCodeSumOfDecryptedText encodedText =
    let lowercaseAsciiCodes = map ord ['a'..'z']
        keys = [[a, b, c] | a <- lowercaseAsciiCodes, b <- lowercaseAsciiCodes, c <- lowercaseAsciiCodes]
        decryptNext :: (Int, Int) -> [Int] -> (Int, Int)
        decryptNext acc @ (maxSumOfAsciiValues, maxEnglishWordCount) key =
            let decoded = [char `xor` k | (char, k) <- zip encodedText (cycle key)]
                recoveredText = words $ map chr decoded
                wordCount = foldl (\cnt w -> if w `elem` mostCommonEnglishWords then cnt + 1 else cnt) 0 recoveredText
            in if wordCount > maxEnglishWordCount then (sum decoded, wordCount) else acc
    in fst $ foldl decryptNext (0, 0) keys


main :: IO()
main = do
    encryptedText <- fmap (splitOn ",") (readFile "../resources/P0059.dat")
    let asciiCodedText = map read encryptedText
    print $ asciiCodeSumOfDecryptedText asciiCodedText
