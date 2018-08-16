import Data.Char
-- main = do
--   putStrLn "What's your first name?"
--   firstname <- getLine
--   name <- getLineputStrLn "What's your last name?"
--   lastname <- getLine
--   let bigFirstName = map toUpper firstname
--       bigLastName = map toUpper lastname
--   putStrLn $ "hey " ++ bigFirstName ++  " " 
--                     ++ bigLastName 
--                     ++ ", how are you?"

main = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words




