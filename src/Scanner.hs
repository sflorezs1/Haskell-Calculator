module Scanner where

    promptLine :: String -> IO String
    promptLine prompt = do
        putStr (prompt ++ ": ")
        getLine
    