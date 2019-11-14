module Parser where

    import Data.List.Split
    import Data.String
    import Data.Maybe
    import Data.List
    import Data.Fixed

    findString :: (Eq a) => [a] -> [a] -> Int
    findString search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

    isInteger :: String -> Bool
    isInteger s = case reads s :: [(Integer, String)] of
        [(_, "")] -> True
        _         -> False
       
    isDouble :: String -> Bool
    isDouble s = case reads s :: [(Double, String)] of
        [(_, "")] -> True
        _         -> False

    isNumeric :: String -> Bool
    isNumeric s = isInteger s || isDouble s
        
    parse :: String -> Double
    parse expression
        |'(' `elem` expression = parseParen expression
        |findString "*-" expression /= -1 = parseOpMinus expression "*-"
        |findString "/-" expression /= -1 = parseOpMinus expression "/-"
        |findString "%-" expression /= -1 = parseOpMinus expression "%-"
        |findString "^-" expression /= -1 = parseOpMinus expression "^-"
        |'+' `elem` expression = parseSum expression
        |'-' `elem` expression = parseRest expression
        |'*' `elem` expression = parseTimes expression
        |'/' `elem` expression = parseDivide expression
        |'%' `elem` expression = parseMod expression 
        |'^' `elem` expression = parsePower expression
        |isNumeric expression = read expression :: Double
        |otherwise = error ("Parse Error: Unrecognized Operator in expression {" ++ expression ++ "}")
    
    parseParen :: String -> Double
    parseParen expression = do
        let idxOpen = openParen expression 0 0
        let idxClose = closeParen (drop idxOpen expression) idxOpen
        let prev = take idxOpen expression
        let next = drop (idxClose + 1) expression
        let mid = show (parse (take (idxClose - idxOpen - 1) (drop (idxOpen + 1) expression)))
        let ret = parse (prev ++ mid ++ next)
        ret
        where 
            openParen :: String -> Int -> Int -> Int
            openParen [] i j = i
            openParen str i j
                |head str == '(' = openParen (tail str) j (j + 1) 
                |otherwise = openParen (tail str) i (j + 1)
            closeParen :: String -> Int -> Int
            closeParen [] j = j
            closeParen str j
                |head str == ')' = j
                |otherwise = closeParen (tail str) (j + 1)
    
    parseOpMinus :: String -> String -> Double
    parseOpMinus expression operator = op (parse (take place expression)) (parse (drop (place + 2) expression))
        where place = findString operator expression
              op :: Double -> Double -> Double
              op num1 num2 -- The error is a lie!!!
                |operator == "*-" = num1 * (-1) * num2
                |operator == "/-" = num1 / num2 * (-1)
                |operator == "%-" = num1 `mod'` (num2 * (-1))
                |operator == "^-" = num1 ** ((-1) * num2)

    parseSum :: String -> Double
    parseSum expression = do
        let splitted = splitOn "+" expression
        if head splitted == "" then sum (map parse (tail splitted))
        else sum (map parse splitted)

    parseRest :: String -> Double
    parseRest expression = do
        let splitted = splitOn "-" expression
        if head splitted == "" then foldl (-) 0 (map parse (tail splitted))
        else foldr ((-) . parse) 0 splitted

    parseTimes :: String -> Double
    parseTimes expression = product (map parse (splitOn "*" expression))
    
    parseDivide :: String -> Double
    parseDivide expression = foldr ((/) . parse) 1 (splitOn "/" expression)

    parseMod :: String -> Double
    parseMod expression = foldr (mod' . parse) 1 (splitOn "%" expression)

    parsePower :: String -> Double
    parsePower expression = foldr ((**) . parse) 1 (splitOn "^" expression)