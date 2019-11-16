module Main where

    import Parser
    import Data.List

    replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
    replace old new list
        |length old > length list = list
        |old == new = list
        |take (length old) list == old = new ++ replace old new (drop (length old) list)
        |otherwise = head list : replace old new (tail list)

    main :: IO ()
    main = do
        putStr "Enter a expression: "
        hola <- getLine
        let expression = replace " " "" (replace "--" "+" hola)
        putStrLn $ "Result: " ++ show (parse expression)