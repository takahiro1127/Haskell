import Control.Monad
import Data.Char

main = interact shortLineOnly

shortLineOnly :: String -> String
shortLineOnly = inlines . filter (\line -> length line < 10) . lines

respondPalindromes :: String -> String
respondPalindromes = unlines . map (\xs -> if isPal xs then "palindrome" else "not a palindrome") . lines

isPal :: String -> Bool
isPal xs = xs == reverse xs

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' name mode f = bracket (openFile name mode)
    (\handle -> hclode handle)
    (\handle -> f handle)