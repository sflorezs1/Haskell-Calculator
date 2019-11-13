module Parser where

    import Data.List.Split

    isInteger s = case reads s :: [(Integer, String)] of
        [(_, "")] -> True
        _         -> False
       
    isDouble s = case reads s :: [(Double, String)] of
        [(_, "")] -> True
        _         -> False

    isNumeric :: String -> Bool
    isNumeric s = isInteger s || isDouble s
        

    -- Do Special Cases (cicles, decisions, etc)
    -- Then do least precedence case
    -- . . .
    -- Then do most precedence case
    -- Check for number
    parse :: String -> Double
    parse expression = do
        if (elem '+' expression) then parseSum expression
        else if (elem '-' expression) then parseRest expression
        else if (isNumeric expression) then (read expression :: Double)
        else error ("ParseError in expression: " ++ expression)

    parseSum :: String -> Double
    parseSum expression = foldr (+) (0) (map parse (splitOn "+" expression))

    parseRest :: String -> Double
    parseRest expression = foldr (-) (0) (map parse (splitOn "-" expression))
    {-
    parseTimes :: String -> Double
    parseTimes expression = do
        foldl (*) (1) mappedExpressions
        where
            mappedExpressions = map parse expressions
            where
                expressions = splitOn "*" expression
    
    parseDivide :: String -> Double
    parseDivide expression = do
        foldl (/) (1) mappedExpressions
        where
            mappedExpressions = map parse expressions
            where
                expressions = splitOn "/" expression
    -}