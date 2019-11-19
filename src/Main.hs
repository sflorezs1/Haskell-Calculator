module Main where

    import Parser

    main :: IO ()
    main = do
        putStr "Enter a expression: "
        usrIn <- getLine
        let expression = replace " " "" (replace "--" "+" usrIn)
        putStrLn $ "Result: " ++ show (parse expression)