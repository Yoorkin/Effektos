{
module Lexer(tokenize,Token(..),TokenKind(..),showTokens) where
import Data.List
}

%wrapper "posn"


$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

$white+                         ;
"//".*                          ;
"/*" .* "*/"                    ;
"true"                          { tok $ \s -> Boolean (read s) }
"false"                         { tok $ \s -> Boolean (read s) }
$digit+                         { tok $ \s -> Number (read s) }
\" .* \"                    { tok $ \s -> String (read s) }
$alpha [$alpha $digit \_ \']*   { tok $ \s -> if s `elem` keywords 
                                           then Symbol s
                                           else Identifier s }
"=="   { tok $ \s -> Symbol s }
"!="   { tok $ \s -> Symbol s }
">="   { tok $ \s -> Symbol s }
"<="   { tok $ \s -> Symbol s }
"->"   { tok $ \s -> Symbol s }
"="    { tok $ \s -> Symbol s }
"+"    { tok $ \s -> Symbol s }
"-"    { tok $ \s -> Symbol s }
"*"    { tok $ \s -> Symbol s }
"/"    { tok $ \s -> Symbol s }
">"    { tok $ \s -> Symbol s }
"<"    { tok $ \s -> Symbol s }
":"    { tok $ \s -> Symbol s }
";"    { tok $ \s -> Symbol s }
"("    { tok $ \s -> Symbol s }
")"    { tok $ \s -> Symbol s }
","    { tok $ \s -> Symbol s }
"|"    { tok $ \s -> Symbol s }
"_"    { tok $ \s -> Symbol s }

{

keywords = 
  [ "let"
  , "in"
  , "rec"
  , "and"
  , "case"
  , "of"
  , "if"
  , "then"
  , "else"
  , "fun"
  , "handle"
  , "with"
  , "resume"
  , "raise"
  , "extern"
  , "effect"
  , "data"
  ]  


tok f (AlexPn _ l c) s = Token l c (length s) (f s)

data Token = Token 
  { posLine :: !Int
  , posColumn :: !Int
  , posLength :: !Int 
  , tokenKind :: TokenKind
  } 

instance Show Token where
  show (Token line col len kind) = "(" ++ show kind ++ ","
                                  ++ show line ++ ":" 
                                  ++ show col ++ "," 
								  ++ show len  
								  ++ ")"

showTokens tokens = 
  case tokens of 
    [] -> "[]"
    (x:xs) -> "[ " ++ foldl' (\acc a -> acc ++ "\n, " ++ show a) (show x) xs ++ "\n]"

data TokenKind
    = Symbol String
    | Identifier String
    | Number Int
    | Boolean Bool
    | String String
    | Eof
    deriving (Show)

tokenize :: String -> [Token]
tokenize = (++ [Token 0 0 0 Eof]) . alexScanTokens

}
