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
    findOpr (x:xs) operators i
        |x `elem` operators = i
        |otherwise = findOpr xs operators (i + 1)

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
        |'(' `elem` expression = parseGrouping expression "(" ")"
        |'[' `elem` expression = parseGrouping expression "[" "]"
        |';' `elem` expression = parseLoop expression
        |'?' `elem` expression = parseIf expression
        |findString "*-" expression /= -1 = parseOpMinus expression "*-"
        |findString "/-" expression /= -1 = parseOpMinus expression "/-"
        |findString "%-" expression /= -1 = parseOpMinus expression "%-"
        |findString "^-" expression /= -1 = parseOpMinus expression "^-"
        |'+' `elem` expression = parseBasic expression "+" 0 (+)
        |'-' `elem` expression = parseBasic expression "-" 0 (-)
        |'*' `elem` expression = parseBasic expression "*" 1 (*)
        |'/' `elem` expression = parseBasic expression "/" 1 (/) 
        |'%' `elem` expression = parseBasic expression "%" 1 mod'
        |'^' `elem` expression = parsePower expression
        |findString "Sinh" expression /= -1 = parseMathFunc expression "Sinh" (sinh . degrees)
        |findString "Cosh" expression /= -1 = parseMathFunc expression "Cosh" (cosh . degrees)
        |findString "Cos" expression /= -1 = parseMathFunc expression "Cos" (cos . degrees)
        |findString "Sin" expression /= -1 = parseMathFunc expression "Sin" (sin . degrees)
        |findString "Tan" expression /= -1 = parseMathFunc expression "Tan" (tan . degrees)
        |findString "Ln" expression /= -1 = parseMathFunc expression "Ln" log
        |otherwise = error ("Parse Error: Unrecognized Operator in expression {" ++ expression ++ "}")
    
    parseGrouping :: String -> String -> String -> Double
    parseGrouping expression groupOpener groupCloser = parse $ prev ++ show (parse (drop (idxOpen + 1) (take idxClose expression))) ++ next
        where
            prev = take idxOpen expression
            next = drop (idxClose + 1) expression
            idxOpen = findOpl expression groupOpener $ fromIntegral $ length expression - 1
            idxClose = findOpr (drop idxOpen expression) groupCloser idxOpen

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
    parseLoop expression = loop initial condition variation expr
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
                |otherwise = 0

    parseMathFunc :: String -> String -> (Double -> Double) -> Double
    parseMathFunc expression opS op = op $ parse $ drop idx expression
        where
            idx = fromIntegral $ length opS + findString opS expression

    parseOpMinus :: String -> String -> Double
    parseOpMinus expression operator = parse $ prev ++ show (op num1 num2) ++ next
        where 
            place = findString operator expression
            idxLeft = findOpl (take place expression) "+-*/()[]%^" (fromIntegral $ length (take place expression) - 1)  
            idxRight = findOpr (drop (place + 2) expression) "+-*/()[]%^" $ place + 2
            num1 = parse $ drop (idxLeft + 1) $ take place expression
            num2 = parse $ drop (place + 2) $ take idxRight expression
            prev = take (idxLeft + 1) expression
            next = drop idxRight expression 
            op :: Double -> Double -> Double
            op num1 num2
                |operator == "*-" = num1 * (-1) * num2 
                |operator == "/-" = num1 / num2 * (-1) 
                |operator == "%-" = num1 `mod'` (num2 * (-1))
                |operator == "^-" = num1 ** ((-1) * num2)
            

    parseBasic :: String -> String -> Double -> (Double -> Double -> Double) -> Double
    parseBasic expression opS baseCase op = foldl op baseCase $ map parse altered
        where altered
                |head splitted == "" = show baseCase : tail splitted
                |otherwise = splitted
                where splitted = splitOn opS expression

    parsePower :: String -> Double
    parsePower expression = foldr ((**) . parse) 1 (splitOn "^" expression)

    parseNumber :: String -> Double
    parseNumber expression
        |number < 1e-7 && number > -1e-7 = 0
        |otherwise = number
        where number = read expression :: Double