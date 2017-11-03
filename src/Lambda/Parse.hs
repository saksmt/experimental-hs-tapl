module Lambda.Parse where

import Text.Parsec hiding (parse)
import Text.Parsec.Language
import Text.Parsec.Expr
import Text.Parsec.String hiding (parse)
import Lambda.Term
import Control.Applicative((<*>), (<$>))
import Control.Monad(join, when)
import qualified Text.ParserCombinators.Parsec.Token as Token
import Debug.Trace
import Data.Char
import Data.Maybe

letterButNotLambda :: Parser Char
letterButNotLambda = satisfy (\x -> isAlpha x && x /= 'λ') <?> "letter"

lambdaLangDef = emptyDef {
  Token.commentLine = "//",
  Token.commentStart = "/*",
  Token.commentEnd = "*/",
  Token.identStart = letterButNotLambda,
  Token.identLetter = alphaNum,
  Token.reservedNames = ["λ"],
  Token.reservedOpNames = [ "\\", ".", "=" ]
}

lexer = Token.makeTokenParser lambdaLangDef

identifier = Token.identifier lexer
reservedOp = Token.reservedOp lexer
reserved = Token.reserved lexer
parens = Token.parens lexer
whiteSpace = Token.whiteSpace lexer
semi = Token.semi lexer

lambdaSign = char 'λ' >> whiteSpace

absOp = lambdaSign <|> reservedOp "\\"
absP' = do
  absOp
  name <- identifier
  reservedOp "."
  body <- exprP
  return $ TAbstraction (VariableName name) body
absP = absP' <|> parens absP'

variableP' = do
  eqSign <- lookAhead (optionMaybe $ try $ identifier >> reservedOp "=")
  when (isJust eqSign) $ parserFail "Unexpected attempt to parse variable assignment"
  name <- identifier
  return $ TVarRef $ VariableName name

variableP = variableP' <|> parens variableP'

operandsP = parens (try totalApp <|> try absP) <|> variableP

totalApp = foldl1 TApplication <$> many1 operandsP

exprP = try totalApp <|> try absP <|> variableP

allOf :: Parser a -> Parser [a]
allOf p = do
  whiteSpace
  v <- many1 p
  eof
  return v

m1 << m2 = do
  x <- m1
  m2
  return x

lineP = do
  whiteSpace
  name <- identifier
  reservedOp "="
  value <- exprP
  return Scope { parent = Nothing, variables = [(VariableName name, value)] }

fileP = mconcat . mconcat <$> allOf (lineP `sepEndBy1` semi)
