-- mobile.hs
-- Exercise 2.29 from SICP

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (javaStyle)

-- Data structure

type Rod = Integer
type Wt  = Integer

data Branch =
     Simple Rod Wt |
     Complex Rod Mobile
     deriving Show

data Mobile =
     Mobile Branch Branch
     deriving Show

{- CFG for mobile representation
   Mobile  --> "Mobile" Branch Branch
   Branch  --> Simple | Complex
   Complex --> "(" "Complex" natural "(" Mobile ")" ")"
   Simple  --> "(" "Simple" natural natural ")"
   
   natural is nonnegative integer
-}

-- Read a list of mobiles from a file and test whetehr each is balanced

main :: IO ()
main = do
     putStr "Please enter filename > "
     inf    <- getLine
     result <- parseFromFile mobiles inf
     case result of
        Left err -> print err
        Right ml -> print (map isBalanced ml)
        

{- 
   isBalanced is the function you have to fix
   Right now it simply says False for all mobiles

 -}

isBalanced :: Mobile -> Bool
isBalanced _ = False
        
---------------------------------------------
-- The Parser
---------------------------------------------
mobiles :: Parser [Mobile]
mobiles = brackets (commaSep mobile)

mobile :: Parser Mobile
mobile = do
      reserved "Mobile"
      b1 <- parens branch
      b2 <- parens branch
      return (Mobile b1 b2)
  <?> "mobile"
  
branch :: Parser Branch
branch =   simple
        <|>complex
        <?>"branch"

simple :: Parser Branch
simple = do
      reserved "Simple"
      r <- natural
      w <- natural
      return (Simple r w)

complex :: Parser Branch
complex = do
      reserved "Complex"
      r <- natural
      m <- parens mobile
      return (Complex r m)



-----------------------------------------------------------
-- The lexer
-----------------------------------------------------------
lexer     = P.makeTokenParser mobileDef

mobileDef  = javaStyle
          { P.reservedNames  = [ "Mobile", "Simple", "Complex"]
          , P.caseSensitive  = True
          }

parens          = P.parens lexer
brackets        = P.brackets lexer
commaSep        = P.commaSep lexer
whiteSpace      = P.whiteSpace lexer
reserved        = P.reserved lexer
natural         = P.natural lexer
