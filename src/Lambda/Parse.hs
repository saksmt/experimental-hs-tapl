module Lambda.Parse where

import Text.Parsec hiding (parse)
import Text.Parsec.String hiding (parse)
import Lambda.Term
import Control.Applicative((<*>), (<$>))
import Control.Monad(join)

data Token = TAbs String | TName String | TPrio [Token] deriving Show

rawNameP = many1 $ letter <|> digit <|> char '-' <|> char '_'

abstractionP = do
  spaces
  char '\\' <|> char 'λ'
  spaces
  name <- rawNameP
  spaces
  char '.'
  return $ TAbs name

nameP :: Parser Token
nameP = do
  spaces
  name <- rawNameP
  spaces
  return $ TName name

prioP = do
  spaces
  char '('
  spaces
  tokens <- tokensP
  spaces
  char ')'
  return $ TPrio tokens

tokenP = do
  spaces
  token <- abstractionP <|> nameP <|> prioP
  spaces
  return token

tokensP :: Parser [Token]
tokensP = do
  spaces
  tokens <- many tokenP
  spaces
  return tokens

parse (TAbs name:rest) = TAbstraction (VariableName name) <$> parse rest
parse [TName name] = Just $ TVarRef $ VariableName name
parse (TName name:rest) = TApplication (TVarRef $ VariableName name) <$> parse rest
parse [TPrio tokens] = parse tokens
parse (TPrio tokens:rest) = Just TApplication <*> parse tokens <*> parse rest
parse [] = Nothing
