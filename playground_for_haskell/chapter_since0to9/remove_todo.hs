import System.IO
import System.Directory
import Data.List

main = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks

    putStrLn "These are youtr TO-DO items:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which one do you want to delete?"
    -- numberString <- getLine
    -- let number = 1--read numberString
    let newTodoItems = unlines $ delete (todoTasks !! 0) todoTasks
    -- (tempName tempHandle) <- openTempFile "." "temp"
    -- hPutStr tempHandle newTodoItems
    -- hClose tempHandle
    
    -- removeFile "todo.txt"
    -- renameFile tempName "todo.txt"