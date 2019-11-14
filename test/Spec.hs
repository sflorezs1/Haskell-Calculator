import Parser

main :: IO ()
main = print (parse "1+(2-5)+3")