import Text.ParserCombinators.Parsec


run :: Show a => Parser a -> String -> IO ()
run p input
    = case (parse p "" input) of
      Left err -> do 
          putStr "parse error at "
          print err
      Right x -> print x 

sentence :: Parser [String]
sentence = do
     words <- sepBy1 word separator
     oneOf ".?!"
     return words

separator :: Parser ()
separator = skipMany1 (space <|> char ',')

word :: Parser String
word = many1 letter
