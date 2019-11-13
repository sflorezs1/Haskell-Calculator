module Main where

    import Parser
    import Data.List

    replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
    replace old new list = do
        if (length old) > (length list) then list
        else if old == new then list
        else if take (length old) list == old then new ++ replace old new (drop (length old) list)
        else ((head (list)) : (replace (old) (new) (tail (list))))

    
    main :: IO ()
    main = do
        putStrLn "Ingrese su wea"
        hola <- getLine
        let expression = replace " " "" (replace "--" "+" hola)
        print $ parse expression