module Parser() where

import Language

import Control.Monad
import Control.Applicative
import Text.ParserCombinators.Parsec hiding ((<|>), many, optional)

lexeme p = p <* many space

parens p = char '(' *> many space *> p <* char ')'

identifier :: Parser String
identifier = (:) <$> lower <*> many (upper<|>lower)

expr :: Parser Expr
expr =   ZeroE <$ (lexeme $ char 'Z')
     <|> SuccE <$ (lexeme $ char 'S') <*> expr
     <|> VarE  <$> (lexeme identifier)
     <?> "Expression"

pat :: Parser Pat
pat  =   ZeroP <$ (lexeme $ char 'Z')
     <|> SuccP <$ (lexeme $ char 'S') <*> pat
     <|> VarP  <$> (lexeme identifier)
     <?> "Pattern"

join :: Parser Join
join =   VarJ <$> identifier <*> (parens $ pat `sepBy` char ',')
     <|> AndJ 
