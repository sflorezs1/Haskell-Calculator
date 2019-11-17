module Parser where

    import Data.List.Split
    import Data.Maybe
    import Data.List
    import Data.Fixed

    degrees :: Double -> Double
    degrees x = (x*pi)/180

    doLogic :: Double -> Double -> String -> Bool
    doLogic a b operator
        |operator == "<" = a < b
        |operator == "<=" = a <= b
        |operator == ">" = a > b
        |operator == ">=" = a >= b
        |operator == "==" = a == b
        |otherwise = error "Not a logic operator"

    findString :: (Eq a) => [a] -> [a] -> Int
    findString search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

    findOpl :: String -> String -> Int -> Int
    findOpl "" _ i = i
    findOpl str operators i
        |last str `elem` operators = i
        |otherwise = findOpl (init str) operators (i - 1)

    findOpr :: String -> String-> Int -> Int
    findOpr "" _ i = i
    findOpr str operators i
        |head str `elem` operators = i
        |otherwise = findOpr (tail str) operators (i + 1)

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
        |isNumeric expression = parseNumber expression
        |'(' `elem` expression = parseGrouping expression '(' ')'
        |'[' `elem` expression = parseGrouping expression '[' ']'
        |';' `elem` expression = parseLoop expression
        |'?' `elem` expression = parseIf expression
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
        |'C' `elem` expression = parseCosine expression
        |'S' `elem` expression = parseSine expression
        |otherwise = error ("Parse Error: Unrecognized Operator in expression {" ++ expression ++ "}")
    
    parseGrouping :: String -> Char -> Char -> Double
    parseGrouping expression grouperOpener grouperCloser = do
        let idxOpen = openParen expression 0 0
        let idxClose = closeParen (drop idxOpen expression) idxOpen
        let prev = take idxOpen expression
        let next = drop (idxClose + 1) expression
        let mid = show (parse (take (idxClose - idxOpen - 1) (drop (idxOpen + 1) expression)))
        let ret = parse (prev ++ mid ++ next)
        ret
        where 
            openParen :: String -> Int -> Int -> Int
            openParen [] i _ = i
            openParen str i j
                |head str == grouperOpener = openParen (tail str) j (j + 1) 
                |otherwise = openParen (tail str) i (j + 1)
            closeParen :: String -> Int -> Int
            closeParen [] j = j
            closeParen str j
                |head str == grouperCloser = j
                |otherwise = closeParen (tail str) (j + 1)

    parseIf :: String -> Double
    parseIf expression
        | doLogic left right operator = parse exprThen
        | otherwise = parse exprElse
        where
            idxQ = findString "?" expression
            idxComparison = findOpl (take (idxQ + 1) expression) "<=>" idxQ
            idxThen = findString ":" expression
            operator
                | (expression !! idxComparison) == '=' = '=' : expression !! (idxComparison - 1) : "" 
                | otherwise = expression !! idxComparison : ""
            idxL = findString operator expression
            left = parse $ take idxL expression
            right = parse $ drop (idxComparison + 1) (take idxQ expression)
            exprThen = drop (idxQ + 1) (take idxThen expression)
            exprElse = drop (idxThen + 1) expression

    parseLoop :: String -> Double
    parseLoop expression = do
        let ret = loop initial condition variation expr - expr
        ret
        where
            idx1 = findString ";" expression
            idx2 = findString ";" (drop (idx1 + 1) expression) + idx1 + 1
            idx3 = findString ";" (drop (idx2 + 1) expression) + idx2 + 1
            initial = parse $ take idx1 expression
            condition = parse $ drop (idx1 + 2) $ take idx2 expression
            variation = parse $ drop (idx2 + 1) $ take idx3 expression
            expr = parse $ drop (idx3 + 1) expression
            operator
                | (expression !! (idx1 + 1)) == '=' = '=' : expression !! (idx1 + 2) : "" 
                | otherwise = expression !! (idx1 + 1) : ""
            loop :: Double -> Double -> Double -> Double -> Double
            loop initial condition variation expression
                |doLogic initial condition operator = expression + loop (initial + variation) condition variation expression
                |otherwise = expression

    parseSine :: String -> Double
    parseSine expression = sin $ degrees $ parse $ drop (idxS + 1) expression
        where
            idxS = findString "S" expression

    parseCosine :: String -> Double
    parseCosine expression = cos $ degrees $ parse $ drop (idxS + 1) expression
        where
            idxS = findString "C" expression
            
    parseOpMinus :: String -> String -> Double
    parseOpMinus expression operator = op (parse (take place expression)) (parse (drop (place + 2) expression))
        where place = findString operator expression
              op :: Double -> Double -> Double
              op num1 num2
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
    parseMod expression = foldl mod' 0 (map parse $splitOn "%" expression)

    parsePower :: String -> Double
    parsePower expression = foldr ((**) . parse) 1 (splitOn "^" expression)

    parseNumber :: String -> Double
    parseNumber expression
        |number < 1e-4 && number > -1e-4 = 0
        |otherwise = number
        where number = read expression :: Double